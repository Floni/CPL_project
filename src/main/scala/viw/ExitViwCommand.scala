package viw

import viw.internals.State
import viw.internals.State.Position

abstract class ExitViwCommand(state: State) extends Command(state) {
  def eval: Option[State] = Some(getEffect.copy(mode = false))

  def getEffect: State
}

case class InsertCommand(state: State) extends ExitViwCommand(state) {
  def getEffect: State = state
}

case class AppendCommand(state: State) extends ExitViwCommand(state) {
  def getEffect: State = state.copy(position = position.copy(line, char + 1))
}

case class OpenCommand(state: State) extends ExitViwCommand(state) {
  def getEffect: State = state.copy(content =
      getLines(0, line + 1) ++
      "\n" ++
      getLines(line + 1, lines),
    Position(line + 1, 0))
}

case class SubstituteCommand(state: State) extends ExitViwCommand(state) {
  def getEffect: State = DeleteCommand(state).eval match {
    case Some(s) => s
    case _ => state
  }
}

case class GoCommand(state: State) extends ExitViwCommand(state) {
  def getEffect: State = state.copy(position = Position(lines - 1, 0))
}

case class InsertInLineCommand(state: State) extends ExitViwCommand(state) {
  def getEffect: State = state.copy(position = Position(line, 0))
}

case class InsertAfterLineCommand(state: State) extends ExitViwCommand(state) {
  def getEffect: State = state.copy(position = Position(line, lineLength(line)))
}

case class ChangeLineCommand(state: State) extends ExitViwCommand(state) {
  def getEffect: State = state.copy(content =
    getContentUpto(position) ++ getLines(line + 1, lineLength(line)))
}