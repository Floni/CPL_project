package viw

import viw.internals.State
import viw.internals.State.Position

abstract class ModifyTextCommand(state: State) extends Command(state)

abstract class DeletionCommand(state: State) extends ModifyTextCommand(state)

case class DeleteCommand(state: State) extends DeletionCommand(state) {
  def eval: Option[State] = {
    updatePasteBuffer(getPositionContent(position))
    Some(state.copy(content =
      getContentUpto(position) ++ getContentFrom(Position(line, char + 1)),
      position = Position(line, if (char == lineLength(line) - 1 && char > 0) char - 1 else char)))
  }
}

case class DeleteBackCommand(state: State) extends DeletionCommand(state) {
  def eval: Option[State] = {
    updatePasteBuffer(getPositionContent(position))
    Some(state.copy(content =
        getContentUpto(position) ++ getContentFrom(Position(line, char + 1)),
      position = position.copy(character = if (char > 0) char - 1 else 0)
    ))
  }
}

case class DeleteLineCommand(state: State) extends DeletionCommand(state) {
  def eval: Option[State] = {
    updatePasteBuffer(Some(contentLines(line).slice(char, lineLength(line))))
    Some(state.copy(content =
      getContentUpto(position) ++ (if (lines > line + 1) getContentFrom(Position(line + 1, 0)) else ""),
      position = position.copy(character = if (char > 0) char - 1 else 0)
    ))
  }
}

case class JoinLineCommand(state: State) extends ModifyTextCommand(state) {
  def eval: Option[State] = {
    if (line < lines - 1) {
      Some(state.copy(content =
        getLines(0, line) ++
          contentLines(line) ++
          " " ++
          contentLines(line + 1) ++
          "\n" ++
          getLines(line + 2, lines),
        position = position.copy(character = lineLength(line))))
    } else {
      Some(state)
    }
  }
}