package viw

import java.lang.Math.min

import viw.internals.State
import viw.internals.State.Position

abstract class SuspendableCommand(state: State) extends Command(state) {
  def eval: Option[State] = None

  def wake(argument: Command) : Option[State]
}

case class DeleteMovementCommand(state: State) extends SuspendableCommand(state) {
  def wake(argument: Command) : Option[State] = argument match {
    case _: DeleteMovementCommand => Some(state.copy(content =
      getLines(0, line) ++
        getLines(line + 1, lines),
      position = Position(min(line, lines - 1), 0)))
    case command: MoveCommand => Some(deleteContentBetween(command.getNewPos))
    case _ => Some(state)
  }

  def deleteContentBetween(nPos: Position) : State = {
    val nPosFirst = (nPos.line < line) || (nPos.line == line && nPos.character < char)
    val fPos = if (nPosFirst) nPos else position
    val ePos = if (nPosFirst) position else nPos
    state.copy(content =
      getLines(0, fPos.line) ++
        contentLines(fPos.line).slice(0, fPos.character) ++
        contentLines(nPos.line)(nPos.character).toString ++
        contentLines(ePos.line).slice(ePos.character + 1, lineLength(ePos.line)) ++
        "\n" ++
        getLines(ePos.line + 1, lines),
      position = fPos)
  }
}

case class ChangeMovementCommand(state: State) extends SuspendableCommand(state) {
  def wake(argument: Command) : Option[State] = argument match {
    case _: ChangeMovementCommand => Some(state.copy(content =
      getLines(0, line) ++
      getLines(line + 1, lines),
      position = Position(min(line, lines - 1), 0),
      mode = false))
    case command: MoveCommand => Some(DeleteMovementCommand(state).deleteContentBetween(command.getNewPos).copy(mode = false))
    case _ => Some(state)
  }
}

case class YankCommand(state: State) extends SuspendableCommand(state) {
  def wake(argument: Command) : Option[State] = argument match {
    case command: MoveCommand => {
      Viw.pasteBuffer = Some(getContentBetween(command.getNewPos))
      Some(state)
    }
    case _ => Some(state)
  }

  def getContentBetween(nPos: Position): String = {
    val nPosFirst = (nPos.line < line) || (nPos.line == line && nPos.character < char)
    val fPos = if (nPosFirst) nPos else position
    val ePos = if (nPosFirst) position else nPos
    if (fPos.line == ePos.line) contentLines(line).slice(fPos.character, ePos.character)
    else contentLines(fPos.line).slice(fPos.character, lineLength(fPos.line)) ++
      "\n" ++
      getLines(fPos.line + 1, ePos.line) ++
      contentLines(ePos.line).slice(0, ePos.character)
  }
}