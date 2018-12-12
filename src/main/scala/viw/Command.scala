package viw

import viw.internals.State
import viw.internals.State.Position

abstract class Command(state: State) {
  val line : Int = state.position.line
  val char : Int = state.position.character
  val contentLines : Vector[String] = state.contentLines
  val position : Position = state.position
  val lines : Int = contentLines.length

  def eval: Option[State]

  def lineLength(line: Int) : Int = contentLines(line).length

  def getLines(start: Int, until: Int) : String = {
    if (start >= lines || until > lines || start == until) ""
    else {
      contentLines.slice(start, until).mkString("\n")
    }
  }
}
