package viw

import viw.internals.State
import viw.internals.State.Position

import Math.min, Math.max

object Viw {
  def processKey(key: String, state: State) : Option[State] = key match{
    case "h" => new MoveLeftCommand(state, key).eval
    case "j" => new MoveDownCommand(state, key).eval
    case "k" => new MoveUpCommand(state, key).eval
    case "l" => new MoveRightCommand(state, key).eval
    case _ => None
  }
}

trait Command {
  def eval: Option[State]
}

abstract class MoveCommand extends Command {}

class MoveLeftCommand(state: State, key: String) extends MoveCommand {
  def eval: Option[State] = {
    val newPos = Position(state.position.line, max(0, state.position.character - 1))
    Option(state.copy(position = newPos))
  }
}

class MoveDownCommand(state: State, key: String) extends MoveCommand {
  def eval: Option[State] = {
    val newPos = if (state.position.line < state.contentLines.length - 1) {
      Position(state.position.line + 1, min(state.contentLines(state.position.line + 1).length, state.position.character))
    } else state.position
    Option(state.copy(position = newPos))
  }
}

class MoveUpCommand(state: State, key: String) extends MoveCommand {
  def eval: Option[State] = {
    val newPos = if (state.position.line > 0) {
      Position(state.position.line - 1, min(state.contentLines(state.position.line - 1).length, state.position.character))
    } else state.position
    Option(state.copy(position = newPos))
  }
}

class MoveRightCommand(state: State, key: String) extends MoveCommand {
  def eval: Option[State] = {
    val newPos = Position(state.position.line, min(state.contentLines(state.position.line).length - 1, state.position.character + 1))
    Option(state.copy(position = newPos))
  }
}