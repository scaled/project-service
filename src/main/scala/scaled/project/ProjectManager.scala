//
// Scaled Project Service - a Scaled framework for grokking projects.
// http://github.com/scaled/project-service/blob/master/LICENSE

package scaled.project

import com.google.common.collect.HashBiMap
import java.io.{File, FileWriter, PrintWriter}
import scala.annotation.tailrec
import scala.collection.mutable.{Map => MMap}
import scala.io.Source
import scaled._

/** Implements [[ProjectService]]. Hides implementation details from clients. */
class ProjectManager (log :Logger, metaSvc :MetaService, pluginSvc :PluginService)
    extends AbstractService with ProjectService {

  // maps from id, srcurl to project root for all known projects
  private val byID  = HashBiMap.create[String,File]()
  private val byURL = HashBiMap.create[String,File]()
  // map from root to name for all known projects (name is not necessarily unique)
  private val toName = MMap[File,String]()

  private val mapFile = metaSvc.metaFile("projects.txt")
  readProjectMap() // TODO: set up a file watch on mapFile; reload on change

  // currently resolved projects
  private val projects = MMap[File,Project]() // TODO: use concurrent maps? need we worry?

  private val finders = pluginSvc.resolvePlugins[ProjectFinderPlugin]("project-finder")

  // TODO: have projects export names, allow switching between projects by names
  // (persist project name to project root, once we see one)
  // then also finding files in other projects? (i.e. C-x C-f -> codex:somefile)

  def didStartup () {
    // TODO!
  }

  def willShutdown () {
    // TODO!
  }

  def projectFor (file :File) = {
    val paths = parents(file.getParentFile)
    resolveProject(paths) getOrElse FileProject.lastDitch(paths.head)
  }

  def projectForId (id :String) = projectInRoot(byID.get(id))
  def projectForSrcURL (srcURL :String) = projectInRoot(byURL.get(srcURL))
  def loadedProjects = projects.values.toSeq
  def knownProjects = toName.toSeq

  // the root passed here may have disappeared in the fullness of time, so validate it
  private def projectInRoot (root :File) =
    if (root == null || !root.exists) None
    else projects.get(root) orElse resolveProject(parents(root))

  private def resolveProject (paths :List[File]) :Option[Project] = {
    // apply each of our finders to the path tree
    val (iprojs, dprojs) = finders.plugins.flatMap(_.apply(paths)).partition(_._2.intelligent)
    // if there are more than one intelligent project matches, complain
    if (!iprojs.isEmpty) {
      if (iprojs.size > 1) log.log(s"Multiple intelligent project matches: ${iprojs.mkString(" ")}")
      Some(openProject(iprojs.head._1, iprojs.head._2))
    }
    // if there are any non-intelligent project matches, use the deepest match
    else if (!dprojs.isEmpty) {
      val deep = dprojs.maxBy(_._1.getPath.length)
      Some(openProject(deep._1, deep._2))
    }
    else None
  }

  private def openProject (root :File, finder :ProjectFinderPlugin) = projects.getOrElse(root, {
    // println(s"Creating ${pf.name} project in $root")
    val proj = metaSvc.injectInstance(finder.projectClass, List(root))
    projects += (root -> proj)

    // add this project to our all-projects maps, and save them if it's new
    val newID = proj.id.map(id => byID.put(id, root) != root).getOrElse(false)
    val newURL = proj.sourceURL.map(url => byURL.put(url, root) != root).getOrElse(false)
    val newName = toName.put(root, proj.name) != Some(proj.name)
    if (newID || newURL || newName) {
      log.log(s"New project in '$root', updating '${mapFile.getName}'.")
      writeProjectMap()
    }

    // println(s"Created $proj")
    proj
  })

  @tailrec private def parents (file :File, accum :List[File] = Nil) :List[File] =
    file.getParentFile match {
      // don't add the file system root to the path; surely there's no way that's a project root
      case null => accum.reverse
      case prnt => parents(prnt, file :: accum)
    }

  private def readProjectMap () {
    if (mapFile.exists) try {
      Source.fromFile(mapFile).getLines.foreach { line => line.split("\t") match {
        case Array(rpath, id, url, name) =>
          val root = new File(rpath)
          if (id   != "none") byID.put(id, root)
          if (url  != "none") byURL.put(url, root)
          if (name != "none") toName.put(root, name)
        case _ => log.log(s"Invalid line in projects.txt: $line")
      }}
    } catch {
      case e :Exception => log.log(s"Failed to read $mapFile", e)
    }
  }

  private def writeProjectMap () {
    import scala.collection.convert.WrapAsScala._
    val roots = byID.values ++ byURL.values ++ toName.keySet
    val out = new PrintWriter(new FileWriter(mapFile))
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
