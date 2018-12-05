package viw

import viw.internals.State
import viw.internals.State.Position

object Viw {
  def processKey(key: String, state: State) : Option[State] = key match{
    case k if k == "h" => CursorMover.moveCursorLeft(state)
    case k if k == "l" => CursorMover.moveCursorRight(state)
    case k if k == "k" => CursorMover.moveCursorUp(state)
    case k if k == "j" => CursorMover.moveCursorDown(state)
    case _ => None
  }
}

// TODO: define abstract command class and subclass it for each (set of) command.

// TODO: reduce code duplication by merging the move functions
// TODO: Do this by calculating the new line and new char pos based on command and return in a general manner

// TODO: Better way to copy (a part of) the state? Or edit this in some way?

// TODO: Vim

object CursorMover {
  def moveCursorLeft(state: State) : Option[State] = {
    if (state.position.character > 0) {
      Option(State(state.content, Position(state.position.line, state.position.character - 1), None, state.mode))
    } else if (state.position.line > 0) {
      val newLine = state.position.line - 1
      Option(State(state.content, Position(newLine, state.contentLines(newLine).length), None, state.mode))
    } else {
      Option(state)
    }
  }

  def moveCursorRight(state: State) : Option[State] = {
    if (state.position.character < state.contentLines(state.position.line).length) {
      Option(State(state.content, Position(state.position.line, state.position.character + 1), None, state.mode))
    } else if (state.position.line < state.contentLines.length) {
      val newLine = state.position.line + 1
      Option(State(state.content, Position(newLine, 0), None, state.mode))
    } else {
      Option(state)
    }
  }

  // TODO: What if the line we move to is shorter than character pos? Just setting it to the length of that line is not
  // TODO: right because when moving again to another line that has length at least as large as the char pos, then the char pos will again be the original
  def moveCursorUp(state: State) : Option[State] = {
    if (state.position.line > 0) {
      Option(State(state.content, Position(state.position.line - 1, state.position.character), None, state.mode))
    } else {Option(state)}
  }

  def moveCursorDown(state: State) : Option[State] = {
    if (state.position.line < state.contentLines.length) {
      Option(State(state.content, Position(state.position.line + 1, state.position.character), None, state.mode))
    } else {Option(state)}
  }

}