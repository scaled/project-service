//
// Scaled Project Service - a Scaled framework for grokking projects.
// https://github.com/scaled/project-service/blob/master/LICENSE

package scaled.project

import java.io.PrintWriter
import java.nio.file.Path
import java.util.concurrent.{CompletableFuture, ExecutorService}
import java.util.{Collections, List => JList}
import org.eclipse.lsp4j._
import org.eclipse.lsp4j.jsonrpc.MessageConsumer
import org.eclipse.lsp4j.jsonrpc.messages.Either
import org.eclipse.lsp4j.jsonrpc.messages.Message
import org.eclipse.lsp4j.launch.LSPLauncher
import org.eclipse.lsp4j.services.LanguageClient
import org.eclipse.lsp4j.services.LanguageServer
import scaled._
import scaled.code.{CodeCompleter, CodeConfig}
import scaled.grammar.GrammarService
import scaled.util.Filler

abstract class LangClient (
  val project :Project, serverCmd :Seq[String]
) extends LanguageClient with AutoCloseable {
  trace(s"Starting ${serverCmd}...")

  private val debugMode = java.lang.Boolean.getBoolean("scaled.debug")
  private val serverProc = new ProcessBuilder(serverCmd.asJList).start();
  private val launcher = LSPLauncher.createClientLauncher(
    this,
    serverProc.getInputStream(),
    serverProc.getOutputStream(),
    exec.bgService,
    consumer => {
      ((message :Message) => { trace(message) ; consumer.consume(message) }) :MessageConsumer
    })

  /** A proxy for talking to the server. */
  val server = launcher.getRemoteProxy()

  /** Provides the server capabilities, once known. */
  val serverCaps = Promise[ServerCapabilities]()

  /** Emitted when the server sends messages. */
  val messages = Signal[MessageParams]()

  /** A user friendly name for this language server (i.e. 'Dotty', 'Eclpse', etc.). */
  def name :String

  override def toString = s"$name langserver"

  private def exec = project.pspace.wspace.exec
  private val grammarSvc = project.pspace.msvc.service[GrammarService]
  private val textSvc = server.getTextDocumentService

  private def init[T] (t :T)(f :T => Unit) = { f(t) ; t }
  private def createClientCaps = init(new ClientCapabilities()) { caps =>
    caps.setTextDocument(init(new TextDocumentClientCapabilities()) { caps =>
      caps.setCompletion(init(new CompletionCapabilities()) { caps =>
        caps.setCompletionItem(init(new CompletionItemCapabilities()) { caps =>
          caps.setSnippetSupport(true)
        })
        // completionItemKind? { valueSet? :CompletionItemKind[] }
        // contextSupport?
      })
      // caps.setHover(init(new HoverCapabilities()) { caps =>
      //   // TODO: contentFormat?: MarkupKind[];
      // })
      // caps.setSignatureHelp(init(new SignatureHelpCapabilities()) { caps =>
      //   // TODO: signatureInformation?: { documentationFormat?: MarkupKind[] }
      // })
      // caps.setDocumentSymbol(init(new DocumentSymbolCapabilities()) { caps =>
      //   // TODO:   symbolKind?: { valueSet?: SymbolKind[] }
      // })
      caps.setSynchronization(init(new SynchronizationCapabilities()) { caps =>
        // TODO: support will save (& wait until)?
        caps.setDidSave(true)
        // TODO: commitCharactersSupport?: boolean
        // TODO: documentationFormat?: MarkupKind[];
      })
    })
    caps.setWorkspace(init(new WorkspaceClientCapabilities()) { caps =>
      // TODO: nothing to set re: workspace capabilities as of yet
    })
  }

  /* init */ {
    val root = project.root.path
    launcher.startListening()
    val initParams = new InitializeParams()
    initParams.setTrace("verbose")
    initParams.setCapabilities(createClientCaps)
    initParams.setRootUri(root.toUri.toString)
    trace(s"Initializing at root: $root")
    project.emitStatus(s"$name langserver initializing...")
    server.initialize(initParams).thenAccept(rsp => {
      serverCaps.succeed(rsp.getCapabilities)
      project.emitStatus(s"$name langserver ready.")
    })
  }

  /** Formats (and styles) a `text` block, appending it to `buffer`. */
  def format (buffer :Buffer, wrapWidth :Int, text :String) :Buffer = {
    text.split(System.getProperty("line.separator")) foreach { line =>
      if (buffer.lines.length > 0 && buffer.lines.last.length > 0) buffer.split(buffer.end)
      val filler = new Filler(wrapWidth)
      filler.append(line)
      val start = buffer.end
      val end = buffer.insert(start, filler.toLines)
      buffer.addStyle(CodeConfig.docStyle, start, end)
    }
    buffer
  }

  /** Formats a styled `code` block using TextMate `scope`, appending it to `buffer`. */
  def format (buffer :Buffer, code :String, scope :String) :Buffer = {
    if (buffer.lines.length > 0 && buffer.lines.last.length > 0) buffer.split(buffer.end)
    val start = buffer.end
    val end = buffer.insert(start, Line.fromText(code))
    grammarSvc.scoper(buffer, scope).foreach(_.rethinkRegion(start.row, end.row+1))
    buffer
  }

  /** Formats a marked `code` block, appending it to `buffer`. */
  def format (buffer :Buffer, code :MarkedString) :Buffer =
    format(buffer, code.getValue, "source." + code.getLanguage)

  /** Formats `either` a text or code block, appending it to `buffer`. */
  def format (buffer :Buffer, wrapWidth :Int, either :Either[String, MarkedString]) :Buffer =
    LSP.toScala(either) match {
      case Left(text) => format(buffer, wrapWidth, text)
      case Right(mark) => format(buffer, mark)
    }

  /** Formats a `docs` string, appending to `buffer`. May contain newlines. */
  def formatDocs (buffer :Buffer, wrapWidth :Int, docs :String) = format(buffer, wrapWidth, docs)

  /** Formats a `detail` string into a signature (to be shown next to the completion text).
    * Any newlines must be removed. */
  def formatSig (detail :String) :LineV = Line.apply(Filler.flatten(detail).take(40))

  /** Converts LSP completion information into Scaled's format. */
  def toChoice (item :CompletionItem) = {
    def firstNonNull (a :String, b :String) = if (a != null) a else if (b != null) b else ???
    new CodeCompleter.Choice(firstNonNull(item.getInsertText, item.getLabel)) {
      override def label = firstNonNull(item.getLabel, item.getInsertText)
      override def sig = Option(item.getDetail).map(formatSig).map(Line.apply)
      override def details (viewWidth :Int) =
        LSP.adapt(textSvc.resolveCompletionItem(item), exec).
        map(item => Option(item.getDocumentation).
          map(formatDocs(Buffer.scratch("*details*"), viewWidth-4, _)))
    }
  }

  private def debug (item :CompletionItem) :CompletionItem = {
    println("AdditionalTextEdits " + item.getAdditionalTextEdits)
    println("Command " + item.getCommand)
    println("Data " + item.getData)
    println("Detail " + item.getDetail)
    println("Docs " + item.getDocumentation)
    println("Filter " + item.getFilterText)
    println("Insert " + item.getInsertText)
    println("Insert Format " + item.getInsertTextFormat)
    println("Kind Format " + item.getKind)
    println("Label " + item.getLabel)
    println("Sort " + item.getSortText)
    println("Edit " + item.getTextEdit)
    item
  }

  /** Adds this lang client to `buffer`, setting up the necessary bits to trigger lang-mode. */
  def addToBuffer (buffer :RBuffer) {
    buffer.state[LangClient]() = this

    buffer.state[CodeCompleter]() = new CodeCompleter() {
      import CodeCompleter._
      def completeAt (window :Window, buffer :Buffer, pos :Loc, point :Loc) = {
        buffer.state.get[Syncer].foreach { _.flushEdits() }
        val pparams = LSP.toTDPP(buffer, pos)
        LSP.adapt(textSvc.completion(pparams), window).map(result => {
          val (items, incomplete) = LSP.toScala(result).fold(
            items => (items, false),
            list => (list.getItems, list.isIncomplete))
          val sorted = items.toSeq.sortBy(it => Option(it.getSortText) || it.getLabel)
          Completion(pos, sorted.map(toChoice))
        })
      }
    }

    // let the lang server know we've opened a file (if it corresponds to a file on disk)
    buffer.store.file.foreach(path => serverCaps.onSuccess(caps => {
      buffer.state[Syncer]() = new Syncer(caps, buffer, path)
    }))
    // TODO: if a file transitions from not having a disk-backed store to having one (i.e. newly
    // created file that is then saved, or file that is save-as-ed), we should tell the lang server
    // about that too
  }

  class Syncer (caps :ServerCapabilities, buffer :RBuffer, path :Path) {
    val uri = path.toUri.toString
    var vers = 1
    def incDocId = {
      vers += 1
      val docId = new VersionedTextDocumentIdentifier(vers)
      docId.setUri(uri) // machine generated API fail...
      docId
    }
    val docId = new TextDocumentIdentifier(uri)
    buffer.state[TextDocumentIdentifier]() = docId

    textSvc.didOpen({
      val langId = uri.substring(uri.lastIndexOf(".")+1) // TODO: what's a real mapping?
      val item = new TextDocumentItem(uri, langId, vers, Line.toText(buffer.lines))
      new DidOpenTextDocumentParams(item)
    })

    // let the server know when we save the buffer
    buffer.dirtyV.onValue { dirty =>
      if (!dirty) textSvc.didSave(new DidSaveTextDocumentParams(docId))
    }

    // send a close event when the buffer is closed
    buffer.killed.onEmit { textSvc.didClose(new DidCloseTextDocumentParams(docId)) }

    // keep the lang server in sync with buffer changes
    val syncType = LSP.toScala(caps.getTextDocumentSync).fold(k => k, o => o.getChange)
    // accumulate edits to syncEdits
    val syncEdits = SeqBuffer[Buffer.Edit]()
    buffer.edited onValue { edit => syncEdits += edit }
    // when enough time elapses after the last edit, trigger a flush
    val debounceTime = 1000L
    buffer.edited.debounce(debounceTime, exec.ui) onEmit flushEdits

    def flushEdits () :Unit = if (syncEdits.size > 0) {
      println(s"flushEdits ${syncEdits.size}")
      syncType match {
        case TextDocumentSyncKind.Incremental =>
          val events = syncEdits.map(toChangeEvent(buffer, _))
          textSvc.didChange(new DidChangeTextDocumentParams(incDocId, events.asJList))
        case TextDocumentSyncKind.Full =>
          val events = Collections.singletonList(
            new TextDocumentContentChangeEvent(Line.toText(buffer.lines)))
          textSvc.didChange(new DidChangeTextDocumentParams(incDocId, events))
        case kind => println(s"Not syncing '${path}': (sync kind $kind)")
      }
      syncEdits.clear()
    }
  }

  protected def toChangeEvent (buffer :RBuffer, edit :Buffer.Edit) = {
    def mkRange (start :Loc, end :Loc) = new Range(LSP.toPos(start), LSP.toPos(end))
    def mkChange (start :Loc, end :Loc, deleteLen :Int, text :String) =
      new TextDocumentContentChangeEvent(mkRange(start, end), deleteLen, text)
    import Buffer._
    edit match {
      case Insert(start, end) =>
        mkChange(start, start, 0, Line.toText(buffer.region(start, end)))
      case Delete(start, end, deleted) =>
        mkChange(start, end, Line.toText(deleted).length, "")
      case Transform(start, end, orig) => {
          val newText = Line.toText(buffer.region(start, end))
          mkChange(start, end, newText.length, newText)
        }
      case edit => throw new AssertionError(s"LSP: unexpected buffer edit: $edit")
    }
  }

  override def close () {
    trace("Shutting down...")
    server.shutdown().thenAccept(res => {
      trace("Shutdown complete.")
      server.exit()
      // serverProc.destroy()
    })
  }

  /**
   * The workspace/applyEdit request is sent from the server to the client to modify resource on
   * the client side.
   */
  override def applyEdit (
    params :ApplyWorkspaceEditParams
  ) :CompletableFuture[ApplyWorkspaceEditResponse] =
    throw new UnsupportedOperationException()

  /**
   * The client/registerCapability request is sent from the server to the client to register for a
   * new capability on the client side. Not all clients need to support dynamic capability
   * registration. A client opts in via the ClientCapabilities.dynamicRegistration property
   */
  override def registerCapability (params :RegistrationParams) :CompletableFuture[Void] =
    throw new UnsupportedOperationException()

  /**
   * The client/unregisterCapability request is sent from the server to the client to unregister a
   * previously register capability.
   */
  override def unregisterCapability (params :UnregistrationParams) :CompletableFuture[Void] =
    throw new UnsupportedOperationException()

  /**
   * The telemetry notification is sent from the server to the client to ask the client to log a
   * telemetry event.
   */
  def telemetryEvent (data :Object) {
    trace(s"telemetryEvent ${data}") // TODO
  }

  /**
   * Diagnostics notifications are sent from the server to the client to signal results of
   * validation runs.
   */
  def publishDiagnostics (pdp :PublishDiagnosticsParams) {
    exec.ui.execute(() => {
      val store = LSP.toStore(pdp.getUri)
      val diags = pdp.getDiagnostics
      project.compiler.asInstanceOf[LangCompiler].gotDiagnostics(store, diags)
    })
  }

  /**
   * The show message notification is sent from a server to a client to ask the client to display a
   * particular message in the user interface.
   */
  def showMessage (params :MessageParams) :Unit = messages.emit(params)

  /**
   * The show message request is sent from a server to a client to ask the client to display a
   * particular message in the user interface. In addition to the show message notification the
   * request allows to pass actions and to wait for an answer from the client.
   */
  def showMessageRequest (params :ShowMessageRequestParams) :CompletableFuture[MessageActionItem] = {
    trace(s"showMessageRequest ${params.getMessage} (${params.getType})")
    for (action <- params.getActions) {
      trace(s"action ${action.getTitle}")
    }
    CompletableFuture.completedFuture(null)
  }

  /**
   * The log message notification is send from the server to the client to ask the client to log a
   * particular message.
   */
  def logMessage (msg :MessageParams) {
    exec.ui.execute(() => {
      project.metaSvc.log.log(s"${msg.getType}: ${msg.getMessage}")
    })
  }

  protected def trace (msg :Any) {
    if (debugMode) println(s"$name langserver: $msg")
  }
}
