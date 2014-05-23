//
// Scaled Project Service - a Scaled framework for grokking projects.
// http://github.com/scaled/project-service/blob/master/LICENSE

package scaled.project

import com.google.common.base.Charsets
import com.google.common.collect.HashBiMap
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

  // maps from id, srcurl to project root for all known projects
  private val byID  = HashBiMap.create[String,Path]()
  private val byURL = HashBiMap.create[String,Path]()
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
    case FileStore(path)       => resolveProject(parents(path.getParent))
    case ZipEntryStore(zip, _) => projects.getOrElseUpdate(zip, new ZipFileProject(zip, metaSvc))
    case _                     => unknownProject
  }
  def projectIn (root :Path) = projectInRoot(root) getOrElse {
    throw Errors.feedback(s"No project in $root")
  }
  def projectForId (id :String) = projectInRoot(byID.get(id))
  def projectForSrcURL (srcURL :String) = projectInRoot(byURL.get(srcURL))
  def loadedProjects = projects.values.toSeq
  def knownProjects = toName.toSeq

  // the root passed here may have disappeared in the fullness of time, so validate it
  private def projectInRoot (root :Path) =
    if (root == null || !Files.exists(root)) None
    else projects.get(root) orElse Some(resolveProject(List(root)))

  private def resolveProject (paths :List[Path]) :Project = {
    // apply each of our finders to the path tree
    val (iprojs, dprojs) = finders.plugins.flatMap(_.apply(paths)).partition(_._2.intelligent)
    // if there are more than one intelligent project matches, complain
    if (!iprojs.isEmpty) {
      if (iprojs.size > 1) log.log(s"Multiple intelligent project matches: ${iprojs.mkString(" ")}")
      openProject(iprojs.head._1, iprojs.head._2.projectClass)
    }
    // if there are any non-intelligent project matches, use the deepest match
    else if (!dprojs.isEmpty) {
      val deep = dprojs.maxBy(_._1.getNameCount)
      openProject(deep._1, deep._2.projectClass)
    }
    else openProject(paths.last, classOf[FileProject])
  }

  private def openProject (root :Path, clazz :Class[_ <: Project]) = projects.getOrElse(root, {
    // println(s"Creating $clazz project in $root")
    val proj = metaSvc.injectInstance(clazz, List(root))
    projects += (root -> proj)

    // add this project to our all-projects maps, and save them if it's new; note that we use
    // forcePut to ensure that if a project previously mapped to some other id or url, we replace
    // it rather than throw an exception
    val newID = proj.id.map(id => byID.forcePut(id, root) != root).getOrElse(false)
    val newURL = proj.sourceURL.map(url => byURL.forcePut(url, root) != root).getOrElse(false)
    val newName = toName.put(root, proj.name) != Some(proj.name)
    if (newID || newURL || newName) {
      log.log(s"New project in '$root', updating '${mapFile.getFileName}'.")
      writeProjectMap()
    }

    // println(s"Created $proj")
    proj
  })

  @tailrec private def parents (file :Path, accum :List[Path] = Nil) :List[Path] =
    file.getParent match {
      // don't add the file system root to the path; surely there's no way that's a project root
      case null => accum.reverse
      case prnt => parents(prnt, file :: accum)
    }

  private def readProjectMap () {
    if (Files.exists(mapFile)) try {
      Files.readAllLines(mapFile).foreach { line => line.split("\t") match {
        case Array(rpath, id, url, name) =>
          val root = Paths.get(rpath)
          if (!Files.exists(root)) log.log(s"Removing obsolete project: $rpath")
          else {
            if (id   != "none") byID.put(id, root)
            if (url  != "none") byURL.put(url, root)
            if (name != "none") toName.put(root, name)
          }
        case _ => log.log(s"Invalid line in projects.txt: $line")
      }}
    } catch {
      case e :Exception => log.log(s"Failed to read $mapFile", e)
    }
  }

  private def writeProjectMap () {
    val roots = byID.values ++ byURL.values ++ toName.keySet
    val out = new PrintWriter(Files.newBufferedWriter(mapFile, Charsets.UTF_8))
    try {
      def orNone (str :String) = if (str == null) "none" else str
      roots foreach { root =>
        val id = byID.inverse.get(root)
        val url = byURL.inverse.get(root)
        val name = toName.getOrElse(root, null)
        if (id != null || url != null || name != null) {
          out.println(s"$root\t${orNone(id)}\t${orNone(url)}\t${orNone(name)}")
        }
      }
    } finally {
      out.close()
    }
  }
}
