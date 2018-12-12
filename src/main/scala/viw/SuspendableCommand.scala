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
    case _: DeleteMovementCommand => Some(state.copy(content = getLines(0, line) ++ "\n" ++ getLines(line + 1, lines), position = Position(min(line, lines - 1), 0)))
    case command: MoveCommand => Some(deleteContentBetween(command.getNewPos))
  }

  def deleteContentBetween(nPos: Position) : State = {
    val nPosFirst = (nPos.line < line) || (nPos.line == line && nPos.character < char)
    val fPos = if (nPosFirst) nPos else position
    val ePos = if (nPosFirst) position else nPos
    state.copy(content =
      getLines(0, fPos.line) ++
        (if (ePos.line > fPos.line && fPos.line > 0) "\n" else "") ++
        contentLines(fPos.line).slice(0, fPos.character) ++
        contentLines(line)(char).toString ++
        contentLines(ePos.line).slice(ePos.character + 1, lineLength(ePos.line)) ++
        (if (ePos.line > fPos.line) "\n" else "") ++
        getLines(ePos.line + 1, lines),
      position = fPos)
  }
}

case class ChangeMovementCommand(state: State) extends SuspendableCommand(state) {
  def wake(argument: Command) : Option[State] = {
    DeleteMovementCommand(state).wake(argument) match {
      case Some(s) => Some(s.copy(mode = false))
      case _ => None
    }
  }
}
