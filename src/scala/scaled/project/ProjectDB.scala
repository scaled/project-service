//
// Scaled Project Service - a Scaled framework for grokking projects.
// https://github.com/scaled/project-service/blob/master/LICENSE

package scaled.project

import java.io.PrintWriter
import java.nio.file.{Files, Path, Paths}
import java.util.HashMap
import java.util.stream.Collectors
import scaled._
import scaled.util.Errors

/** Maintains metadata for all projects known to a project space. */
class ProjectDB (wsroot :Path, log :Logger) {
  import Project._

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
    def save () :Unit = Files.write(
      metaDir.resolve("info.txt"), Seq(rootToString(root)) ++ ids.map(_.deflate))
  }

  /** Current metadata for all known projects. */
  val toInfo = {
    val imap = new HashMap[Root,Info]()
    // read info.txt for all known projects and map them by root
    Files.list(psdir).collect(Collectors.toList[Path]).foreach { pdir =>
      if (Files.isDirectory(pdir)) try {
        val lines = List() ++ Files.readAllLines(pdir.resolve("info.txt"))
        val root = rootFromString(lines.head)
        imap.put(root, Info(root, pdir.getFileName.toString, lines.tail.flatMap(inflateId).toSeq))
      } catch {
        case e :Throwable => log.log(s"Failed to resolve info for $pdir: $e")
      }
    }
    imap.values foreach { _.map() } // map all the infos we read
    imap
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
    // write this project's id info its metadata dir
    info.save()

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
        // remove the project's info.txt file
        Files.deleteIfExists(pdir.resolve("info.txt"))
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
        ninfo.save()
      }
      // TODO: name change?
  }
}
