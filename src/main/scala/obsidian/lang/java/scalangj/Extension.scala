package obsidian.lang.java.scalangj

import typings.vscode.mod as vscode
import typings.vscode.anon.Dispose
import typings.vscode.Thenable

import scala.collection.immutable
import scala.util.*
import scala.scalajs.js
import scala.scalajs.js.annotation.JSExportTopLevel
import scala.scalajs.js.UndefOr

import concurrent.ExecutionContext.Implicits.global
import obsidian.lang.java.scalangj.Parser.parseCompilationUnit
import obsidian.lang.java.scalangj.Pretty.prettyPrint
import typings.std.PromiseLike

object extension {
  @JSExportTopLevel("activate")
  def activate(context: vscode.ExtensionContext): Unit = {

    println(
      """Congratulations, your extension "scalangj" is now active!"""
    )

    def showHello(): js.Function1[Any, Any] =
      (arg) => {
        vscode.window.showInputBox().toFuture.onComplete {
          case Success(input) => vscode.window.showInformationMessage(s"Hello World $input!")
          case Failure(e)     => println(e.getMessage)
        }
      }
    
    def getAST(context: vscode.ExtensionContext): js.Function1[Any, PromiseLike[UndefOr[scala.Any]]] = (arg: Any) => {
      vscode.window.activeTextEditor.toOption match
        case Some(editor) => {
            parseCompilationUnit(
              editor.document.getText(
                vscode.Range(
                  editor.selection.start.line,
                  editor.selection.start.character,
                  editor.selection.`end`.line,
                  editor.selection.`end`.character
                )
              )
            ) match {
              case Left(error_msg) => vscode.window.showInformationMessage(error_msg)
              case Right(cu)       => vscode.window.showInformationMessage(prettyPrint(cu))
            }
          }
        case None =>
          vscode.window.showInformationMessage("No active editor")
    }

    val commands = List(
      ("extension.helloWorld", showHello()),
      ("extension.getAST", getAST(context))
    )

    commands.foreach { case (name, fun) =>
      context.subscriptions.push(
        vscode.commands
          .registerCommand(name, fun)
          .asInstanceOf[Dispose]
      )
    }

  }
}