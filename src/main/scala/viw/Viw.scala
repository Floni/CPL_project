package viw

import viw.internals.State
import viw.internals.State.Position

import Math.min, Math.max



object Viw {
  def processKey(key: String, state: State) : Option[State] = key match{
    case "h" => new MoveLeftCommand(state).eval
    case "j" => new MoveDownCommand(state).eval
    case "k" => new MoveUpCommand(state).eval
    case "l" => new MoveRightCommand(state).eval
    case _ => None
  }
}

abstract  class Command(state: State) {
  def eval: Option[State]
}

abstract class MoveCommand(state: State) extends Command(state) {}

case class MoveLeftCommand(state: State) extends MoveCommand(state) {
  def eval: Option[State] = {
    val newPos = Position(state.position.line, max(0, state.position.character - 1))
    Option(state.copy(position = newPos))
  }
}

case class MoveDownCommand(state: State) extends MoveCommand(state) {
  def eval: Option[State] = {
    val newPos = if (state.position.line < state.contentLines.length - 1) {
      Position(state.position.line + 1, min(state.contentLines(state.position.line + 1).length, state.position.character))
    } else state.position
    Option(state.copy(position = newPos))
  }
}

case class MoveUpCommand(state: State) extends MoveCommand(state) {
  def eval: Option[State] = {
    val newPos = if (state.position.line > 0) {
      Position(state.position.line - 1, min(state.contentLines(state.position.line - 1).length, state.position.character))
    } else state.position
    Option(state.copy(position = newPos))
  }
}

case class MoveRightCommand(state: State) extends MoveCommand(state) {
  def eval: Option[State] = {
    val newPos = Position(state.position.line, min(state.contentLines(state.position.line).length - 1, state.position.character + 1))
    Option(state.copy(position = newPos))
  }
}