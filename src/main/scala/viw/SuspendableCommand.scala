package viw

import java.lang.Math.min

import viw.internals.State
import viw.internals.State.Position

abstract class SuspendableCommand(state: State) extends Command(state) {
  def eval: Option[State] = None

  def wake(argument: Command) : Command
}

case class DeleteMovementCommand(state: State) extends SuspendableCommand(state) {
  def wake(argument: Command) : Command = DeleteMovementResultCommand(state, argument)
}

case class DeleteMovementResultCommand(state: State, cmd : Command) extends ModifyTextCommand(state) {
  def eval: Option[State] =  cmd match {
    case _: DeleteMovementCommand =>
      updatePasteBuffer(Some(getLines(line, line + 1)))
      Some(state.copy(content =
        getLines(0, line) ++
          getLines(line + 1, lines),
        position = Position(min(line, lines - 1), 0)))
    case command: MoveCommand => Some(deleteContentBetween(command.getNewPos(position)))
    case _ => Some(state)
  }

  def deleteContentBetween(nPos: Position) : State = {
    val nPosFirst = (nPos.line < line) || (nPos.line == line && nPos.character < char)
    val fPos = if (nPosFirst) nPos else position
    val ePos = if (nPosFirst) position else nPos
    updatePasteBuffer(Some(getContentBetween(fPos, ePos)))
    state.copy(content =
      getLines(0, fPos.line) ++
        contentLines(fPos.line).slice(0, fPos.character) ++
        contentLines(ePos.line).slice(ePos.character, lineLength(ePos.line)) ++
        "\n" ++
        getLines(ePos.line + 1, lines),
      position = fPos)
  }
}

case class ChangeMovementCommand(state: State) extends SuspendableCommand(state) {
  def wake(argument: Command) : Command = ChangeMovementResultCommand(state, argument)
}

case class ChangeMovementResultCommand(state: State, cmd : Command) extends ModifyTextCommand(state) {
  def eval: Option[State] = cmd match {
    case _: ChangeMovementCommand =>
      updatePasteBuffer(Some(getLines(line, line + 1)))
      Some(state.copy(content =
        getLines(0, line) ++
          getLines(line + 1, lines),
        position = Position(min(line, lines - 1), 0),
        mode = false))
    case command: MoveCommand =>
      Some(DeleteMovementResultCommand(state, cmd).deleteContentBetween(command.getNewPos(position)).copy(mode = false))
    case _ => Some(state)
  }
}

case class YankCommand(state: State) extends SuspendableCommand(state) {
  def wake(argument: Command) : Command = YankResultCommand(state, argument)
}

case class YankResultCommand(state: State, cmd : Command) extends Command(state) {
  def eval: Option[State] = cmd match {
    case command: MoveCommand => {
      Viw.pasteBuffer = Some(getContentBetween(command.getNewPos(position), position))
      Some(state)
    }
    case _ => Some(state)
  }
}

case class CountCommand(count : Int)(state: State) extends SuspendableCommand(state) {
  def wake(argument: Command) : Command = argument match {
    case _ => MoveCountResultCommand(state, count, argument)
  }
}

case class MoveCountResultCommand(state: State, count: Int, cmd: Command) extends MoveCommand(state) {
  def getNewPos(pos: State.Position): State.Position = cmd match {
    case command: MoveCommand => (0 until count).foldLeft(pos)((p, _) => command.getNewPos(p))
    case _ => pos
  }
}

case class IndentIncCommand(state: State) extends SuspendableCommand(state) {
  def wake(argument: Command): Command = IndentIncResultCommand(state, argument)
}

case class IndentIncResultCommand(state: State, cmd: Command) extends ModifyTextCommand(state) {
  def eval: Option[State] = cmd match {
    case _: IndentIncCommand =>
      Some(state.copy(content = getLines(0, line) ++ "  " ++ getLines(line, lines),
        position = Position(line, char + 2)))
    case command: MoveCommand => {
      val nLine = command.getNewPos(position).line
      val fLine = if (line < nLine) line else nLine
      val eLine = if (line < nLine) nLine else line
      Some(state.copy(content =
        getLines(0, fLine) ++ IncIndent(fLine, eLine, "") ++ getLines(eLine + 1, lines),
        position = Position(line, char + 2)))
    }
    case _ => Some(state)
  }

  def IncIndent(l1 : Int, l2 : Int, acc : String) : String = {
    val newLine = "  " ++ getLines(l1, l1 + 1)
    if (l1 == l2) acc ++ newLine
    else IncIndent(l1 + 1, l2, acc ++ newLine)
  }
}

case class IndentDecCommand(state: State) extends SuspendableCommand(state) {
  def wake(argument: Command): Command = IndentDecResultCommand(state, argument)
}

case class IndentDecResultCommand(state: State, cmd: Command) extends ModifyTextCommand(state) {
  def eval: Option[State] = cmd match {
    case _: IndentDecCommand =>
      val newLine = decIndent(line, line, "")
      Some(state.copy(content =
        getLines(0, line) ++ newLine ++ getLines(line + 1, 0),
        position = Position(line, char + newLine.length - lineLength(line) - 1)))
    case command: MoveCommand => {
      val nLine = command.getNewPos(position).line
      val fLine = if (line < nLine) line else nLine
      val eLine = if (line < nLine) nLine else line
      val newLines = decIndent(fLine, eLine, "")
      Some(state.copy(content =
        getLines(0, fLine) ++ newLines ++ getLines(eLine + 1, lines),
        position = Position(line, char + newLines.split("\n")(line - fLine).length - lineLength(line) - 1)))
    }
    case _ => Some(state)
  }

  def decIndent(l1 : Int, l2 : Int, acc : String) : String = {
    val firstChar = contentLines(line).indexWhere(c => c != ' ')
    val newLine = getLines(l1, l1 + 1).slice(min(firstChar, 2), lineLength(l1) + 1)
    if (l1 == l2) acc ++ newLine
    else decIndent(l1 + 1, l2, acc ++ newLine)
  }
}