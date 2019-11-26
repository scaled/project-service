//
// Scaled Project Service - a Scaled framework for grokking projects.
// https://github.com/scaled/project-service/blob/master/LICENSE

package scaled.project

import scaled._

/**
 * A minor mode providing fns for interacting with an LSP client, when one is configured in a
 * buffer.
 */
@Minor(name="lang", tags=Array("project"), stateTypes=Array(classOf[LangClient]),
       desc="""A minor mode that provides LSP client fns.""")
class LangMinorMode (env :Env) extends MinorMode(env) {

  private def client = buffer.state.get[LangClient].getOrElse {
    throw new IllegalStateException(s"No LSP client configured in buffer: '$buffer'")
  }

  override def keymap = super.keymap.
    bind("lang-exec-command", "C-c C-l x")

  @Fn("""Queries for the name of an 'execute command' supported by this buffer's language
         server and instructs the server to execute it.""")
  def langExecCommand () :Unit = {
    if (client.execCommands.isEmpty) window.popStatus(s"Lang server exposes no commands.")
    else {
      val comp = Completer.from(client.execCommands)
      window.mini.read("Command:", "", commandRing, comp).onSuccess { cmd =>
        client.execCommand(cmd)
              .onSuccess(obj => window.emitStatus(s"Executed: $cmd"))
              .onFailure(window.exec.handleError)
      }
    }
  }

  private val commandRing = new Ring(8)
}
