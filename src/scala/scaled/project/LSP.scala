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

  def textDocItem(uri :String, langId :String, vers :Int, text :String) :TextDocumentItem =
    new TextDocumentItem(uri, langId, vers, text)

  def toStore (uri :String) :Store = toStore(new URI(uri))
  def toStore (uri :URI) :Store = Store(Paths.get(uri))

  def toPos (loc :Loc) = new Position(loc.row, loc.col)

  def getName (loc :Location) = Paths.get(new URI(loc.getUri).getPath()).getFileName.toString

  def fromPos (pos :Position) = Loc.apply(pos.getLine, pos.getCharacter)

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

  def adapt[T] (res :CompletableFuture[T], window :Window) :Future[T] = adapt(res, window.exec)
}
