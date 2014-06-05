//
// Scaled Project Service - a Scaled framework for grokking projects.
// http://github.com/scaled/project-service/blob/master/LICENSE

package scaled.project

import com.google.common.base.Charsets
import java.io.PrintWriter
import java.nio.file.{Files, Path, Paths}
import scala.annotation.tailrec
import scala.collection.mutable.{Map => MMap}
import scala.io.Source
import scaled._
import scaled.util.Errors

/** Implements [[ProjectService]]. Hides implementation details from clients. */
class ProjectManager (log :Logger, metaSvc :MetaService, pluginSvc :PluginService)
    extends AbstractService with ProjectService {
  import scala.collection.convert.WrapAsScala._
  import Project._

  // maps from id to project root for all known projects
  private val byId  = MMap[Id,Path]()
  // map from root to name for all known projects (name is not necessarily unique)
  private val toName = MMap[Path,String]()

  private val mapFile = metaSvc.metaFile("projects.txt")
  readProjectMap() // TODO: set up a file watch on mapFile; reload on change

  // currently resolved projects
  private val projects = MMap[Path,Project]() // TODO: use concurrent maps? need we worry?

  private val finders = pluginSvc.resolvePlugins[ProjectFinderPlugin]("project-finder")

  private val unknownProject = new Project(metaSvc) {
    val fileCompleter = Completer.file
    def name = "<unknown>"
    def root = Paths.get("")
  }

  // TODO: have projects export names, allow switching between projects by names
  // (persist project name to project root, once we see one)
  // then also finding files in other projects? (i.e. C-x C-f -> codex:somefile)

  def didStartup () {
    // TODO!
  }

  def willShutdown () {
    // TODO!
  }

  def projectFor (store :Store) = store match {
    case FileStore(path)       => resolveByPaths(parents(path.getParent))
    case ZipEntryStore(zip, _) => projects.getOrElseUpdate(zip, new ZipFileProject(zip, metaSvc))
    case _                     => unknownProject
  }
  def projectIn (root :Path) = projectInRoot(root) getOrElse {
    throw Errors.feedback(s"No project in $root")
  }
  def projectFor (id :Project.Id) = byId.get(id).flatMap(projectInRoot).orElse(resolveById(id))
  def loadedProjects = projects.values.toSeq
  def knownProjects = toName.toSeq

  // the root passed here may have disappeared in the fullness of time, so validate it
  private def projectInRoot (root :Path) =
    if (root == null || !Files.exists(root)) None
    else projects.get(root) orElse Some(resolveByPaths(List(root)))

  private def resolveByPaths (paths :List[Path]) :Project = {
    def open (root :Path, thunk :(MetaService => Project)) =
      projects.getOrElse(root, mapProject(thunk(metaSvc)))

    val (iprojs, dprojs) = finders.plugins.flatMap(_.apply(paths)).partition(_._2)
    // if there are more than one intelligent project matches, complain
    if (!iprojs.isEmpty) {
      if (iprojs.size > 1) log.log(s"Multiple intelligent project matches: ${iprojs.mkString(" ")}")
      open(iprojs.head._1, iprojs.head._3)
    }
    // if there are any non-intelligent project matches, use the deepest match
    else if (!dprojs.isEmpty) {
      val deep = dprojs.maxBy(_._1.getNameCount)
      open(deep._1, deep._3)
    }
    // if all else fails, create a FileProject for the root
    else open(paths.last, _.injectInstance(classOf[FileProject], List(paths.last)))
  }

  private def resolveById (id :Project.Id) :Option[Project] = {
    val iter = finders.plugins.iterator
    while (iter.hasNext) iter.next.apply(id) match {
      case Some(thunk) => return Some(mapProject(thunk(metaSvc)))
      case None => // keep on keepin' on
    }
    None
  }

  private def mapProject (proj :Project) :Project = {
    projects += (proj.root -> proj)

    // add this project to our all-projects maps, and save them if it's new; note that we use
    // forcePut to ensure that if a project previously mapped to some other id or url, we replace
    // it rather than throw an exception
    var newID = false
    proj.ids.foreach { id =>
      if (byId.put(id, proj.root) != Some(proj.root)) newID = true
    }
    val newName = toName.put(proj.root, proj.name) != Some(proj.name)
    if (newID || newName) {
      log.log(s"New project in '${proj.root}', updating '${mapFile.getFileName}'.")
      writeProjectMap()
    }

    // println(s"Created $proj")
    proj
  }

  @tailrec private def parents (file :Path, accum :List[Path] = Nil) :List[Path] =
    file.getParent match {
      // don't add the file system root to the path; surely there's no way that's a project root
      case null => accum.reverse
      case prnt => parents(prnt, file :: accum)
    }

  private def readProjectMap () {
    if (Files.exists(mapFile)) try {
      Files.readAllLines(mapFile).foreach { line => line.split("\t") match {
        case Array(rpath, name, ids @ _*) =>
          val root = Paths.get(rpath)
          if (!Files.exists(root)) log.log(s"Removing obsolete project: $rpath")
          else {
            if (name != "none") toName.put(root, name)
            ids flatMap(inflateId) foreach { id => byId.put(id, root) }
          }
        case _ => log.log(s"Invalid line in projects.txt: $line")
      }}
    } catch {
      case e :Exception => log.log(s"Failed to read $mapFile", e)
    }
  }

  private def writeProjectMap () {
    val roots = Set() ++ byId.values ++ toName.keySet
    val rootToIds = byId.keys.groupBy(byId)
    val out = new PrintWriter(Files.newBufferedWriter(mapFile, Charsets.UTF_8))
    try {
      roots foreach { root =>
        val ids = rootToIds.getOrElse(root, Seq())
        val name = toName.get(root)
        if (!ids.isEmpty || !name.isEmpty) {
          out.print(root)
          out.print("\t")
          out.print(name.getOrElse("none"))
          ids foreach { id => out.print("\t") ; out.print(id.deflate) }
          out.println()
        }
      }
    } finally {
      out.close()
    }
  }
}
