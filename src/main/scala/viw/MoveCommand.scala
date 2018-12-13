package viw

import java.lang.Math.{max, min}

import viw.internals.State
import viw.internals.State.Position

abstract class MoveCommand(state: State) extends Command(state) {
  def eval: Option[State] = Some(state.copy(position = getNewPos(state.position)))

  def getNewPos(pos : Position): Position
}

case class MoveLeftCommand(state: State) extends MoveCommand(state) {
  def getNewPos(pos : Position): Position = Position(pos.line, max(0, pos.character - 1))
}

case class MoveDownCommand(state: State) extends MoveCommand(state) {
  def getNewPos(pos : Position): Position = if (pos.line < lines - 1)
    Position(pos.line + 1, min(lineLength(pos.line + 1), pos.character))
  else pos
}

case class MoveUpCommand(state: State) extends MoveCommand(state) {
  def getNewPos(pos : Position): Position =
    if (pos.line> 0) Position(pos.line - 1, min(lineLength(pos.line - 1), pos.character))
  else pos
}

case class MoveRightCommand(state: State) extends MoveCommand(state) {
  def getNewPos(pos : Position): Position = Position(pos.line, min(lineLength(pos.line) - 1, pos.character + 1))
}

abstract class MoveWordCommand(state: State) extends MoveCommand(state) {
  def whitespacePos(pos: Position) : Int =
    contentLines(pos.line).indexOf(' ', pos.character)
  def characterPos(pos: Position) : Int =
    contentLines(pos.line).slice(whitespacePos(pos), lineLength(pos.line)).indexWhere(c => c != ' ') + whitespacePos(pos)
}

case class NextWordCommand(state: State) extends MoveWordCommand(state) {
  def getNewPos(pos : Position): Position = {
    if (whitespacePos(pos) == -1 || characterPos(pos) == -1) {
      if (pos.line == lines - 1) {
        return pos
      } else {
        val nextLine = Position(pos.line + 1, 0)
        return if (contentLines(pos.line + 1)(0) != ' ')
          nextLine
        else NextWordCommand(state).getNewPos(nextLine)
      }
    }
    Position(pos.line, characterPos(pos))
  }
}

case class BackWordCommand(state: State) extends MoveWordCommand(state) {
  def getNewPos(pos : Position): Position = {
    val prevWhitespace = contentLines(pos.line).slice(0, pos.character).lastIndexOf(' ')
    val wordStart = if (prevWhitespace > 0) prevWhitespace + 1 else 0
    val firstChar = contentLines(pos.line).indexWhere(c => c != ' ')
    if (pos.character == wordStart) {
      if (pos.character == firstChar) {
        if (pos.line == 0) return pos
        return getPrevWordStart(pos.line - 1, lineLength(pos.line - 1)) // Start of last word on previous line
      }
      getPrevWordStart(pos.line, wordStart) // Start of word before current word
    }
    else {
      Position(pos.line, wordStart) // Start of current word
    }
  }

  def getPrevWordStart(l: Int, c: Int): Position = {
    val prevWordEnd = contentLines(l).slice(0, c).lastIndexWhere(c => c != ' ')
    val prevWordWhitespace = contentLines(l).slice(0, prevWordEnd).lastIndexOf(' ')
    val prevWordStart = if (prevWordWhitespace > 0) prevWordWhitespace + 1 else 0
    Position(l, prevWordStart)
  }
}

case class EndWordCommand(state: State) extends MoveWordCommand(state) {
  def getNewPos(pos : Position): Position = {
    if (whitespacePos(pos) == -1) {
      if (pos.character != lineLength(pos.line) - 1) Position(pos.line, lineLength(pos.line) - 1)
      else if (pos.line < lines - 1) EndWordCommand(state).getNewPos(Position(pos.line + 1, 0))
      else pos
    } else {
      if (whitespacePos(pos) - 1 == pos.character)
        EndWordCommand(state).getNewPos(Position(pos.line, characterPos(pos)))
      else Position(pos.line, whitespacePos(pos) - 1)
    }
  }
}

case class EndLineCommand(state: State) extends MoveCommand(state) {
  def getNewPos(pos : Position): Position = Position(pos.line, lineLength(pos.line) - 1)
}

case class StartLineCommand(state: State) extends MoveCommand(state) {
  def getNewPos(pos : Position): Position = Position(pos.line, 0)
}

// TODO: replace line & char by pos.line & pos.character
case class MatchBracketCommand(state: State) extends MoveCommand(state) {
  // Recursive function to find matching bracket position
  def iteratePos(cPos : Position, counter: Int, bracket: Char, mBracket: Char, openBracket: Boolean): Position = {
    if (counter == 0) return cPos
    val nPos = if (openBracket) nextPos(cPos) else prevPos(cPos)
    nPos match {
      case Some((l : Int, c : Int)) =>
        if (contentLines(l)(c) == bracket) iteratePos(Position(l, c), counter + 1, bracket, mBracket, openBracket)
        else if (contentLines(l)(c) == mBracket) iteratePos(Position(l, c), counter - 1, bracket, mBracket, openBracket)
        else iteratePos(Position(l, c), counter, bracket, mBracket, openBracket)
      case None => position
    }
  }

  def getNewPos(pos : Position): Position = {
    val openBrackets = List('[', '(', '{')
    val closeBrackets = List(']', ')', '}')
    val brackets = openBrackets ++ closeBrackets

    if (!brackets.contains(contentLines(pos.line)(pos.character))) {
      if (!brackets.exists(c => contentLines(pos.line).contains(c))) {
        // Current char not bracket and no brackets in line
        pos
      } else {
        // Go to first bracket in the current line
        Position(pos.line, contentLines(pos.line).indexWhere(c => brackets.contains(c)))
      }
    }
    else {
      val bracket = contentLines(pos.line)(pos.character) // Bracket char
      val openBracket = openBrackets.contains(bracket) // Bool indicating whether it's an open bracket
      // Get the matching bracket character
      val mBracket = if (openBracket) closeBrackets(openBrackets.indexOf(bracket))
      else openBrackets(closeBrackets.indexOf(bracket))
      iteratePos(pos, 1, bracket, mBracket, openBracket)
    }
  }

  // Returns the next position after the given position, returns None if there is no position after the current position
  def nextPos(pos: Position) : Option[(Int, Int)] = {
    if (pos.character < lineLength(pos.line) - 1) Some((pos.line, pos.character + 1))
    else if (pos.line < lines - 1) Some((pos.line + 1, 0))
    else None
  }

  // Returns the position before the given position, returns None if there is no position before the current position
  def prevPos(pos: Position) : Option[(Int, Int)] = {
    if (pos.character > 0) Some((pos.line, pos.character - 1))
    else if (pos.line > 0) Some((pos.line - 1, lineLength(pos.line - 1) - 1))
    else None
  }
}