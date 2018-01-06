//
// Scaled Project Service - a Scaled framework for grokking projects.
// https://github.com/scaled/project-service/blob/master/LICENSE

package scaled.project

import java.io.{InputStream, PrintWriter}
import java.net.URI
import java.nio.file.Path
import java.util.concurrent.{CompletableFuture, ExecutorService}
import java.util.{Collections, List => JList, HashMap}
import org.eclipse.lsp4j._
import org.eclipse.lsp4j.jsonrpc.messages.{Either, Message}
import org.eclipse.lsp4j.jsonrpc.{Launcher, MessageConsumer}
import org.eclipse.lsp4j.services.{LanguageClient, LanguageServer}
import scaled._
import scaled.code.{CodeCompleter, CodeConfig}
import scaled.grammar.GrammarService
import scaled.util.{BufferBuilder, Close, Filler, SubProcess}

object LangClient {

  /** Extracts the `LangClient` from `buffer` state. */
  def apply (buffer :Buffer) :LangClient = buffer.state.get[LangClient].getOrElse {
    throw new IllegalStateException(s"No LSP client configured in buffer: '$buffer'")
  }

  class Component (project :Project) extends Project.Component {
    private val toClose = Close.bag()
    private val clients = new HashMap[String,Future[LangClient]]()
    private lazy val plugins = project.metaSvc.service[PluginService].
      resolvePlugins[LangPlugin]("langserver")

    private def rootPath = project.root.path
    private def pluginForSuff (suff :String) :Option[LangPlugin] =
      plugins.plugins.filter(_.canActivate(rootPath)).find(_.suffs(rootPath).contains(suff))

    /** Resolves the lang client for `suff` code (i.e. `java`, `scala`, etc.). This lazily creates
      * and caches the language server.
      */
    def clientFor (suff :String) :Option[Future[LangClient]] =
      Option(clients.get(suff)) orElse pluginForSuff(suff).map(plugin => {
        val client = plugin.createClient(project)
        plugin.suffs(rootPath).foreach { suff => clients.put(suff, client) }
        client onSuccess { client =>
          // pass lang client messages along to project
          client.messages.onValue { m => project.emitStatus(s"${m.getType}: ${m.getMessage}") }
          toClose += client
          // TODO: close lang clients if all buffers with their suff are closed
        }
        client onFailure project.exec.handleError
        client
      })

    override def addToBuffer (buffer :RBuffer) {
      // add a lang client if one is available
      val name = buffer.store.name
      val suff = name.substring(name.lastIndexOf('.')+1).toLowerCase
      clientFor(suff).map(_.onSuccess(_.addToBuffer(buffer)))
    }


    override def describeSelf (bb :BufferBuilder) {
      if (!clients.isEmpty) {
        bb.addSubHeader("Lang Clients")
        // ugly hack to provide info for already resolved clients
        def info (client :Future[LangClient]) :String = {
          var info = "<resolving>"
          client.onSuccess(client => info = client.name)
          info
        }
        bb.addKeysValues(clients.entrySet.map(ent => (s"${ent.getKey}: ", info(ent.getValue))))
      }
    }

    override def close () :Unit = toClose.close()
  }
}

abstract class LangClient (
  val project :Project, serverCmd :Seq[String]
) extends LanguageClient with AutoCloseable {

  private val debugMode = java.lang.Boolean.getBoolean("scaled.debug")
  trace(s"Starting ${serverCmd}...")
  private val serverProc = new ProcessBuilder(serverCmd.asJList).start();

  // read and pass along stderr
  SubProcess.reader(serverProc.getErrorStream,
                    line => System.err.println(s"STDERR: $line"),
                    _.printStackTrace(System.err)).start()

  // for debugging, it can sometimes be useful to record a transcript of the raw data we got from
  // the language server (particularly when the server sends invalid JSON RPC, whee!)
  private val transOut = new java.io.ByteArrayOutputStream()
  private def record (in :InputStream) = new java.io.FilterInputStream(in) {
    override def read (target :Array[Byte], off :Int, len :Int) = {
      val got = super.read(target, off, len)
      transOut.write(target, off, got)
      got
    }
    override def read() = {
      val got = super.read()
      transOut.write(got)
      got
    }
  }

  private val launcher = Launcher.createLauncher(
    this,
    langServerClass.asInstanceOf[Class[LanguageServer]],
    serverProc.getInputStream, // record(serverProc.getInputStream),
    serverProc.getOutputStream(),
    exec.bgService,
    consumer => {
      ((message :Message) => { trace(message) ; consumer.consume(message) }) :MessageConsumer
    })

  protected def langServerClass :Class[_] = classOf[LanguageServer]

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
  private val wspaceSvc = server.getWorkspaceService

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
    // TEMP: for Ensime/Scala which doesn't yet support rootUri, sigh
    initParams.setRootPath(root.toString)
    // TODO: can we get our real PID via a Java API? Ensime fails if we don't send something, sigh
    initParams.setProcessId(0)
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
        LSP.adapt(textSvc.resolveCompletionItem(item), project.exec).
        map(item => Option(item.getDocumentation).
          map(formatDocs(Buffer.scratch("*details*"), viewWidth-4, _)))
    }
  }

  /** Returns the fully qualified name for `sym`. Defaults to C style: `qualifier.name`. */
  def fqName (sym :SymbolInformation) = sym.getContainerName match {
    case null => sym.getName
    case cont => s"${cont}.${sym.getName}"
  }

  /** Formats a symbol name for use during completion. Scaled convention is `name:qualifier`. */
  def formatSym (sym :SymbolInformation) = sym.getContainerName match {
    case null => sym.getName
    case cont => s"${sym.getName}:${cont}"
  }

  /** Visits location `loc` in `window`.
    * @param name the name to use for the visiting buffer if this turns out to be a "synthetic"
    * location (one for which the source is provided by the language server).
    */
  def visitLocation (name :String, loc :Location, window :Window) {
    val uri = new URI(loc.getUri)
    val point = LSP.fromPos(loc.getRange.getStart)
    if (uri.getScheme == "file") {
      val symView = window.focus.visitFile(LSP.toStore(uri))
      symView.point() = point
    } else {
      fetchContents(loc, window.exec).onSuccess(source => {
        val initState = State.init(classOf[LangClient], this) ::
          State.init(classOf[TextDocumentIdentifier], new TextDocumentIdentifier(loc.getUri)) ::
          project.bufferState(modeFor(loc))
        val store = Store.text(name, source, project.root.path)
        val buf = project.pspace.wspace.createBuffer(store, initState, true)
        val symView = window.focus.visit(buf)
        symView.point() = point
      }).onFailure(err => {
        window.popStatus(err.getMessage)
      })
    }
  }

  /** Provides the major editing mode source code fetched from the language server via
    * [[fetchContents]]. */
  def modeFor (loc :Location) :String = "text"

  /** Fetches the contents for a "synthetic" location, one hosted by the language server. */
  def fetchContents (loc :Location, exec :Executor) :Future[String] = {
    Future.failure(new Exception("No support for fetching contents.\n" + loc))
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

  /** Adds this lang client to `buffer`, stuffing various things into the buffer state that enable
    * code smarts. */
  def addToBuffer (buffer :RBuffer) {
    buffer.state[Analyzer]() = new LangAnalyzer(this, project)

    buffer.state[CodeCompleter]() = new CodeCompleter() {
      import CodeCompleter._
      def completeAt (window :Window, buffer :Buffer, pos :Loc, point :Loc) = {
        buffer.state.get[Syncer].foreach { _.flushEdits() }
        val pparams = LSP.toTDPP(buffer, pos)
        LSP.adapt(textSvc.completion(pparams), window.exec).map(result => {
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
      val item = new TextDocumentItem(uri, LSP.langId(uri), vers, Line.toText(buffer.lines))
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
      case Transform(start, end, orig) =>
        val newText = Line.toText(buffer.region(start, end))
        mkChange(start, end, newText.length, newText)
      case edit => throw new AssertionError(s"LSP: unexpected buffer edit: $edit")
    }
  }

  override def close () {
    val transcript = transOut.toByteArray()
    if (transcript.length > 0) {
      trace("-- Session transcript: --")
      trace(new String(transcript))
      trace("-- End transcript --")
    }

    trace("Shutting down...")
    server.shutdown().whenComplete((res, err) => {
      trace("Shutdown complete.")
      server.exit()
      // give the langserver five seconds to shutdown, then stick a fork in it
      exec.bg.schedule(5000L, () => if (serverProc.isAlive) serverProc.destroy())
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
    import Analyzer._
    exec.ui.execute(() => {
      val store = LSP.toStore(pdp.getUri)
      val diags = pdp.getDiagnostics
      project.notes(store)() = Seq() ++ diags.map(diag => Note(
        store,
        Region(LSP.fromPos(diag.getRange.getStart), LSP.fromPos(diag.getRange.getEnd)),
        diag.getMessage,
        diag.getSeverity match {
          case DiagnosticSeverity.Hint => Hint
          case DiagnosticSeverity.Information => Info
          case DiagnosticSeverity.Warning => Warning
          case DiagnosticSeverity.Error => Error
        }))
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
