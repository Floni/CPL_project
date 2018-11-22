package viw.internals

import org.scalajs.dom
import viw._

import scala.scalajs.js
import scala.scalajs.js.annotation.{JSExport, JSExportTopLevel}

@JSExportTopLevel("Runner")
object Runner {

  @JSExport
  def processKey(ev: dom.KeyboardEvent,
                 contents: String,
                 position: js.Array[Int],
                 selection: js.Dynamic,
                 mode: Boolean): js.UndefOr[js.Dictionary[Any]] = {
    import js.JSConverters._

    val p = State.Position(position(0), position(1))

    def pointToPosition(p: js.Dynamic) =
      State.Position(p.row.asInstanceOf[Int], p.column.asInstanceOf[Int])

    val selectionPositions =
      (pointToPosition(selection.start), pointToPosition(selection.end))

    val selectionOpt =
      if (selectionPositions._1 == selectionPositions._2) None
      else Some(selectionPositions)

    val result =
      Viw.processKey(ev.key, State(contents, p, selectionOpt, mode))

    result match {
      case Some(state) =>
        val selectionArray = state.selection
          .map { s =>
            js.Array(js.Array(s._1.line, s._1.character),
                     js.Array(s._2.line, s._2.character))
          }
          .getOrElse(js.undefined)

        js.Dictionary(
          "content" -> state.content,
          "position" -> js.Array(state.position.line, state.position.character),
          "selection" -> selectionArray,
          "mode" -> state.mode
        )
      case None => js.undefined
    }
  }

}
