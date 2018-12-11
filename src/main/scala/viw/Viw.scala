package viw

import viw.internals.State
import viw.internals.State.Position
import Math.min
import Math.max

import scala.collection.mutable.ListBuffer

object Viw {
  var history : ListBuffer[State => Command] = new ListBuffer[State => Command]()

  var pasteBuffer : Option[String] = None

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
    "o" -> OpenCommand,
    "s" -> SubstituteCommand,
    "G" -> GoCommand,
    "I" -> InsertInLineCommand,
    "A" -> InsertAfterLineCommand,
    "C" -> ChangeLineCommand,
    "." -> RepeatCommand,
    "p" -> PasteCommand,
    "P" -> PasteBehindCommand
  )

  def updateHistory(command: State => Command): Unit = {
    history += command
    ()
  }

  def processKey(key: String, state: State) : Option[State] = {
    if (commandMap.contains(key)) {
      val command = commandMap(key)
      updateHistory(command)
      command(state).eval
    } else {
      Some(state)
    }
  }
}

abstract class Command(state: State) {
  val line : Int = state.position.line
  val char : Int = state.position.character
  val contentLines : Vector[String] = state.contentLines
  val position : Position = state.position
  val lines : Int = contentLines.length

  def eval: Option[State]

  def lineLength(line: Int) : Int = contentLines(line).length

  def getLines(start: Int, until: Int) : String = {
    if (start >= lines || until > lines) ""
    else {
      contentLines.slice(start, until).mkString("\n")
    }
  }
}

abstract class MoveCommand(state: State) extends Command(state) {
  def eval: Option[State] = Some(state.copy(position = getNewPos))

  def getNewPos: Position
}

case class MoveLeftCommand(state: State) extends MoveCommand(state) {
  def getNewPos: Position = Position(state.position.line, max(0, char - 1))
}

case class MoveDownCommand(state: State) extends MoveCommand(state) {
  def getNewPos: Position = if (line < lines - 1)
    Position(line + 1, min(lineLength(line + 1), char))
    else position
}

case class MoveUpCommand(state: State) extends MoveCommand(state) {
  def getNewPos: Position = if (line> 0)
    Position(line - 1, min(lineLength(line - 1), char))
    else position
}

case class MoveRightCommand(state: State) extends MoveCommand(state) {
  def getNewPos: Position = Position(line, min(lineLength(line) - 1, char + 1))
}

abstract class MoveWordCommand(state: State) extends MoveCommand(state) {
  val whitespacePos : Int = contentLines(line).indexOf(' ', char)
  val characterPos : Int = contentLines(line).slice(whitespacePos, lineLength(line)).indexWhere(c => c != ' ') + whitespacePos
}

case class NextWordCommand(state: State) extends MoveWordCommand(state) {
  def getNewPos: Position = {
    if (whitespacePos == -1 || characterPos == -1) {
      if (line == lines - 1) {
        return position
      } else {
        val nextLine = Position(line + 1, 0)
        return if (contentLines(line + 1)(0) != ' ')
          nextLine
        else NextWordCommand(state.copy(position = nextLine)).getNewPos
      }
    }
    Position(line, characterPos)
  }
}

case class BackWordCommand(state: State) extends MoveWordCommand(state) {
  def getNewPos: Position = {
    val prevWhitespace = contentLines(line).slice(0, char).lastIndexOf(' ')
    val wordStart = if (prevWhitespace > 0) prevWhitespace + 1 else 0
    val firstChar = contentLines(line).indexWhere(c => c != ' ')
    if (char == wordStart) {
      if (char == firstChar) {
        if (line == 0) return position
        return getPrevWordStart(line - 1, lineLength(line - 1)) // Start of last word on previous line
      }
      getPrevWordStart(line, wordStart) // Start of word before current word
    }
    else {
      Position(line, wordStart) // Start of current word
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
  def getNewPos: Position = {
    if (whitespacePos == -1) {
      return Position(line, lineLength(line) - 1)
    }
    Position(line, whitespacePos - 1)
  }
}

case class EndLineCommand(state: State) extends MoveCommand(state) {
  def getNewPos: Position = Position(line, lineLength(line) - 1)
}

case class StartLineCommand(state: State) extends MoveCommand(state) {
  def getNewPos: Position = Position(line, 0)
}

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

  def getNewPos: Position = {
    val openBrackets = List('[', '(', '{')
    val closeBrackets = List(']', ')', '}')
    val brackets = openBrackets ++ closeBrackets

    if (!brackets.contains(contentLines(line)(char))) {
      if (!brackets.exists(c => contentLines(line).contains(c))) {
        // Current char not bracket and no brackets in line
        position
      } else {
        // Go to first bracket in the current line
        Position(line, contentLines(line).indexWhere(c => brackets.contains(c)))
      }
    }
    else {
      val bracket = contentLines(line)(char) // Bracket char
      val openBracket = openBrackets.contains(bracket) // Bool indicating whether it's an open bracket
      // Get the matching bracket character
      val mBracket = if (openBracket) closeBrackets(openBrackets.indexOf(bracket))
        else openBrackets(closeBrackets.indexOf(bracket))
      iteratePos(position, 1, bracket, mBracket, openBracket)
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

abstract class ModifyTextCommand(state: State) extends Command(state)

abstract class DeletionCommand(state: State) extends ModifyTextCommand(state)

case class DeleteCommand(state: State) extends DeletionCommand(state) {
  def eval: Option[State] = {
    Viw.pasteBuffer = Some(contentLines(line)(char).toString)
    Some(state.copy(content =
      getLines(0, line) ++
        contentLines(line).slice(0, char) ++
        contentLines(line).slice(char + 1, lineLength(line)) ++
        getLines(line + 1, lines),
      position = Position(line, if (char == lineLength(line) - 1) char - 1 else char)
    ))
  }
}

case class DeleteBackCommand(state: State) extends DeletionCommand(state) {
  def eval: Option[State] = {
      Viw.pasteBuffer = Some(contentLines(line)(char).toString)
      Some(state.copy(content =
        getLines(0, line) ++
          contentLines(line).slice(0, char) ++
          contentLines(line).slice(char + 1, lineLength(line)) ++
          getLines(line + 1, lines),
        position = position.copy(character = if (char > 0) char - 1 else 0)
      ))
  }
}

case class DeleteLineCommand(state: State) extends DeletionCommand(state) {
  def eval: Option[State] = {
    Viw.pasteBuffer = Some(contentLines(line).slice(char, lineLength(line)))
    Some(state.copy(content =
      getLines(0, line) ++
        (if (char > 0) contentLines(line).slice(0, char) else "") ++
        getLines(line + 1, lines),
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

case class InsertCommand(state: State) extends Command(state) {
  def eval: Option[State] = Some(state.copy(mode = false))
}

case class AppendCommand(state: State) extends Command(state) {
  def eval: Option[State] = Some(state.copy(position = position.copy(line, char + 1), mode = false))
}

case class OpenCommand(state: State) extends Command(state) {
  def eval: Option[State] = Some(state.copy(
    content = getLines(0, line + 1) ++
      "\n" * 2 ++
      getLines(line + 1, lines),
    Position(line + 1, 0),
    mode = false))
}

case class SubstituteCommand(state: State) extends Command(state) {
  def eval: Option[State] = Some(state.copy(
    content = getLines(0, line) ++
      contentLines(line).slice(0, char) ++
      contentLines(line).slice(char + 1, contentLines(line).length) ++
      getLines(line + 1, lines),
    position = position.copy(character = if (char == lineLength(line) - 1) char - 1 else char),
    mode = false))
}

case class GoCommand(state: State) extends Command(state) {
  def eval: Option[State] = Some(state.copy(position = Position(lines - 1, 0), mode = false))
}

case class InsertInLineCommand(state: State) extends Command(state) {
  def eval: Option[State] = Some(state.copy(position = position.copy(character = 0), mode = false))
}

case class InsertAfterLineCommand(state: State) extends Command(state) {
  def eval: Option[State] = Some(state.copy(position = position.copy(character = lineLength(line)), mode = false))
}

case class ChangeLineCommand(state: State) extends Command(state) {
  def eval: Option[State] = Some(state.copy(content =
      getLines(0, line) ++
      contentLines(line).slice(0, char) ++
      getLines(line + 1, lineLength(line)),
    mode = false))
}

case class RepeatCommand(state: State) extends Command(state) {
  def eval: Option[State] = {
    Viw.history.reverseIterator.find(_(state).isInstanceOf[ModifyTextCommand]) match {
      case Some(cmd) => cmd(state).eval
      case _ => Some(state)
    }
  }
}

case class PasteCommand(state: State) extends Command(state) {
  def eval: Option[State] = Viw.pasteBuffer match {
    case Some(s) => Some(state.copy(content =
      getLines(0, line) ++
      contentLines(line).slice(0, char + 1) ++
      s ++
      (if (lineLength(line) > char + 1) contentLines(line).slice(char + 1, lineLength(line)) else "") ++
      getLines(line + 1, lines),
      position = Position(line, char + 1)))
    case None => Some(state)
  }
}

case class PasteBehindCommand(state: State) extends Command(state) {
  def eval: Option[State] = Viw.pasteBuffer match {
    case Some(s) => Some(state.copy(content =
      getLines(0, line) ++
        contentLines(line).slice(0, char) ++
        s ++
        contentLines(line).slice(char, lineLength(line)) ++
        getLines(line + 1, lines)))
    case None => Some(state)
  }
}