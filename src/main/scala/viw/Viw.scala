package viw

import viw.internals.State
import viw.internals.State.Position

import Math.min, Math.max

object Viw {

  val commandMap: Map[String, State => Command] = Map(
    "h" -> MoveLeftCommand,
    "j" -> MoveDownCommand,
    "k" -> MoveUpCommand,
    "l" -> MoveRightCommand,
    "w" -> NextWordCommand,
    "b" -> BackWordCommand,
    "e" -> EndWordCommand,
    "$" -> EndLineCommand,
    "0" -> StartLineCommand,
    "%" -> MatchBracketCommand,
    "x" -> DeleteCommand,
    "X" -> DeleteBackCommand,
    "D" -> DeleteLineCommand,
    "J" -> JoinLineCommand,
    "i" -> InsertCommand,
    "a" -> AppendCommand,
    "o" -> OpenCommand
  )

  def processKey(key: String, state: State) : Option[State] = {
    if (commandMap.contains(key)) {
      commandMap(key)(state).eval
    } else {
      Some(state)
    }
  }
}

abstract class Command(state: State) {
  def eval: Option[State]

  val line = state.position.line
  val char = state.position.character
  val contentLines = state.contentLines
  val position = state.position
}

abstract class MoveCommand(state: State) extends Command(state) {}

case class MoveLeftCommand(state: State) extends MoveCommand(state) {
  def eval: Option[State] = {
    val newPos = Position(state.position.line, max(0, char - 1))
    Some(state.copy(position = newPos))
  }
}

case class MoveDownCommand(state: State) extends MoveCommand(state) {
  def eval: Option[State] = {
    val newPos = if (state.position.line < contentLines.length - 1) {
      Position(line + 1, min(contentLines(line + 1).length, char))
    } else state.position
    Some(state.copy(position = newPos))
  }
}

case class MoveUpCommand(state: State) extends MoveCommand(state) {
  def eval: Option[State] = {
    val newPos = if (state.position.line > 0) {
      Position(line - 1, min(contentLines(line - 1).length, char))
    } else state.position
    Some(state.copy(position = newPos))
  }
}

case class MoveRightCommand(state: State) extends MoveCommand(state) {
  def eval: Option[State] = {
    val newPos = Position(line, min(contentLines(line).length - 1, char + 1))
    Some(state.copy(position = newPos))
  }
}

abstract class MoveWordCommand(state: State) extends MoveCommand(state) {
  def words(line: String): Array[String] = line.split(' ')
}

case class NextWordCommand(state: State) extends MoveWordCommand(state) {
  def eval: Option[State] = {
    // TODO: implement
    val whitespace = contentLines(line).indexOf(' ', char)
    val character = contentLines(line).indexOf(' ', char)
    Some(state)
  }
}

case class BackWordCommand(state: State) extends MoveWordCommand(state) {
  def eval: Option[State] = {
    Some(state)
  }
}

case class EndWordCommand(state: State) extends MoveWordCommand(state) {
  def eval: Option[State] = {
    Some(state)
  }
}

case class EndLineCommand(state: State) extends MoveCommand(state) {
  def eval: Option[State] = Some(state.copy(position = position.copy(character = contentLines(line).length - 1)))
}

case class StartLineCommand(state: State) extends MoveCommand(state) {
  def eval: Option[State] = Some(state.copy(position = position.copy(character = 0)))
}

case class MatchBracketCommand(state: State) extends MoveCommand(state) {
  def eval: Option[State] = {
    // TODO: implement
    Some(state)
  }
}

case class DeleteCommand(state: State) extends Command(state) {
  def eval: Option[State] = {
    Some(state)
  }
}

case class DeleteBackCommand(state: State) extends Command(state) {
  def eval: Option[State] = {
    // TODO: implement
    Some(state)
  }
}

case class DeleteLineCommand(state: State) extends Command(state) {
  def eval: Option[State] = {
    // TODO: implement
    Some(state)
  }
}

case class JoinLineCommand(state: State) extends Command(state) {
  def eval: Option[State] = {
    // TODO: implement
    Some(state)
  }
}

case class InsertCommand(state: State) extends Command(state) {
  def eval: Option[State] = Some(state.copy(mode = false))
}

case class AppendCommand(state: State) extends Command(state) {
  def eval: Option[State] = Some(state.copy(position = position.copy(line, char + 1), mode = false))
}

case class OpenCommand(state: State) extends Command(state) {
  def eval: Option[State] = Some(state.copy(
    content = (contentLines.slice(0, line + 1).flatten
      ++ Vector("\n")
      ++ contentLines.slice(line + 1, contentLines.length)).mkString(""),
    Position(line + 1, 0),
    mode = false))
}