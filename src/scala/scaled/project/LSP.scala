//
// Scaled Project Service - a Scaled framework for grokking projects.
// https://github.com/scaled/project-service/blob/master/LICENSE

package scaled.project

import java.net.URI
import java.nio.file.Paths
import java.util.concurrent.CompletableFuture
import org.eclipse.lsp4j._
import org.eclipse.lsp4j.jsonrpc.messages.Either
import scaled._
import scaled.util.Errors

/** Helpers to make it easier to work with lsp4j types. */
object LSP {

  case class URILoc (uri :URI, range :Range) {
    def name :String = Paths.get(uri.getPath()).getFileName.toString
    def store = Store(Paths.get(uri))
  }
  case object URILoc {
    def apply (loc :Location) :URILoc = URILoc(new URI(loc.getUri), loc.getRange)
    def apply (link :LocationLink) :URILoc = URILoc(new URI(link.getTargetUri), link.getTargetRange)
  }

  def textDocItem(uri :String, langId :String, vers :Int, text :String) :TextDocumentItem =
    new TextDocumentItem(uri, langId, vers, text)

  def toStore (uri :String) :Store = toStore(new URI(uri))
  def toStore (uri :URI) :Store = Store(Paths.get(uri))

  def toPos (loc :Loc) = new Position(loc.row, loc.col)

  def fromPos (pos :Position) = Loc.apply(pos.getLine, pos.getCharacter)
  def fromRange (range :Range) = Region(fromPos(range.getStart), fromPos(range.getEnd))

  def toScala[A,B] (either :Either[A, B]) =
    if (either.isLeft) Left(either.getLeft) else Right(either.getRight)

  def langId (uri :String) = uri.substring(uri.lastIndexOf(".")+1) // TODO: what's a real mapping?

  def docId (buffer :Buffer) = buffer.state.get[TextDocumentIdentifier] getOrElse {
    throw Errors.feedback("Buffer not saved to a file?")
  }

  def toTDPP (buffer :Buffer, pos :Loc) = new TextDocumentPositionParams(docId(buffer), toPos(pos))

  def adapt[T] (res :CompletableFuture[T], exec :Executor) :Future[T] = {
    val promise = exec.uiPromise[T]
    res.handle[Unit]((result, error) => {
      if (error != null) promise.fail(error)
      else promise.succeed(result)
    })
    promise
  }
}
