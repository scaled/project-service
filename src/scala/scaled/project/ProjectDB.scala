//
// Scaled Project Service - a Scaled framework for grokking projects.
// https://github.com/scaled/project-service/blob/master/LICENSE

package scaled.project

import java.io.PrintWriter
import java.nio.file.{Files, Path, Paths}
import java.util.stream.Collectors
import java.util.{Map => JMap, HashMap}
import scaled._
import scaled.util.Errors

/** Maintains metadata for all projects known to a project space. */
class ProjectDB (wsroot :Path, log :Logger) {
  import Project._
  private val codec = new Codec()

  /** A file that contains info on all of our projects. */
  private val configFile = wsroot.resolve("Config").resolve("projects.conf")

  /** The directory in which metadata is stored for known projects. */
  val psdir = Files.createDirectories(wsroot.resolve("Projects"))
  /** The directory in which metadata is stored for depend projects. */
  val dsdir = Files.createDirectories(wsroot.resolve("Depends"))

  /** All known projects mapped by id. */
  val byId = new HashMap[Id,Root]()

  /** Metadata for a named project. */
  case class Info (root :Root, name :String, ids :Seq[Id]) {
    val rootName = (root, name)
    def metaDir = psdir.resolve(name)
    def map () :Unit = ids foreach { id => byId.put(id, root) }
    def unmap () :Unit = ids foreach { id => byId.remove(id) }
  }

  /** Current metadata for all known projects. */
  val toInfo = new HashMap[Root,Info]()

  /*ctor*/ {
    // load metadata from our config file
    if (Files.exists(configFile)) {
      ConfigFile.read(configFile).map(readInfo).foreach { info => toInfo.put(info.root, info) }
    }
    // if we have no config file, potentially migrate from the old per-project info style
    else {
      val vtab = new StringBuilder().append(11.toChar).toString
      val codec = new Codec(vtab, vtab)
      // read info.txt for all known projects and map them by root
      Files.list(psdir).collect(Collectors.toList[Path]).foreach { pdir =>
        if (Files.isDirectory(pdir)) try {
          val lines = List() ++ Files.readAllLines(pdir.resolve("info.txt"))
          val root = codec.readRoot(lines.head)
          val info = Info(root, pdir.getFileName.toString, lines.tail.flatMap(codec.readId).toSeq)
          toInfo.put(root, info)
        } catch {
          case e :Throwable => log.log(s"Failed to resolve info for $pdir: $e")
        }
      }
      writeConfig()
    }
    toInfo.values foreach { _.map() } // map all the infos we read
  }

  /** Returns the directory into which `proj` should store its metadata. */
  def metaDir (proj :Project) :Path = toInfo.get(proj.root) match {
    // if it's an incidental project (e.g. a random Maven dependency), use an id-based dir
    case null => dsdir.resolve(proj.idName)
    // if this is a named project in this workspace, use a dir based on its name
    case info => info.metaDir
  }

  /** Adds `proj` to this database.
    * @return true if added, false if project was already added. */
  def add (proj :Project) :Boolean = if (toInfo.containsKey(proj.root)) false else {
    // if this project's name is already in use by another project, tack -N onto it
    val names = toInfo.values.map(_.name).filter(_ startsWith proj.name).toSet
    val name = if (!names(proj.name)) proj.name else {
      var ext = 1
      while (names(proj.name + s"-$ext")) ext += 1
      proj.name + s"-$ext"
    }

    val info = Info(proj.root, name, proj.ids)
    toInfo.put(proj.root, info)
    info.map()

    // move this project's metadir from Depends into Projects (or create it if it doesn't exist)
    val ddir = dsdir.resolve(proj.idName) ; val pdir = metaDir(proj)
    if (Files.exists(ddir)) Files.move(ddir, pdir)
    else Files.createDirectories(ddir)
    // write out our project metadata db
    writeConfig()

    true
  }

  /** Removes `proj` from this database.
    * @return false if project was not in database, true if it was removed. */
  def remove (proj :Project) :Boolean = toInfo.remove(proj.root) match {
    case null => false
    case info =>
      info.unmap()
      // grab the project's metadata directory then remove it from toName
      val pdir = metaDir(proj)
      if (Files.exists(pdir)) {
        // move this project's metadir out of Projects back into Depends
        Files.move(pdir, dsdir.resolve(proj.idName))
        // write our project metadata db
        writeConfig()
      }
      true
  }

  /** Checks that the metadata for `proj` is up to date; updating it if needed. */
  def checkInfo (proj :Project) :Unit = toInfo.get(proj.root) match {
    case null => // it's not a named project, nothing to update
    case oinfo =>
      val newids = proj.ids
      if (oinfo.ids != newids) {
        val ninfo = oinfo.copy(ids = newids)
        log.log(s"Updating ids for $proj: $ninfo")
        toInfo.put(proj.root, ninfo)
        oinfo.unmap()
        ninfo.map()
        writeConfig()
      }
      // TODO: name change?
  }

  private def readInfo (lines :Seq[String]) :Info =
    Info(codec.readRoot(lines(0)), lines(1), lines.drop(2).flatMap(codec.readId).toSeq)
  private def showInfo (info :Info) :Seq[String] =
    Seq(codec.showRoot(info.root), info.name) ++ info.ids.map(codec.showId)

  private def writeConfig () {
    ConfigFile.write(configFile, toInfo.values.toSeq.sortBy(_.name).map(showInfo))
  }
}
