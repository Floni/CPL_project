package viw

import viw.internals.State
import viw.internals.State.Position

abstract class Command(state: State) {
  val line : Int = state.position.line
  val char : Int = state.position.character
  val contentLines : Vector[String] = state.contentLines
  val position : Position = state.position
  val lines : Int = contentLines.length

  def eval: Option[State]

  def lineLength(line: Int) : Int = contentLines(line).length

  def getPositionContent(pos: Position) : Option[String] = {
    if (lineLength(pos.line) <= pos.character) None
    else Some(contentLines(pos.line)(pos.character).toString)
  }

  def getLines(start: Int, until: Int) : String = {
    if (start >= lines || until > lines || start == until) ""
    else contentLines.slice(start, until).mkString("\n") ++ "\n"
  }

  // Gets the content between pos1 (inclusive) and pos2 (exclusive)
  def getContentBetween(pos1: Position, pos2 : Position): String = {
    val pos1First = (pos1.line < pos2.line) || (pos1.line == pos2.line && pos1.character < pos2.character)
    val fPos = if (pos1First) pos1 else pos2
    val ePos = if (pos1First) pos2 else pos1
    if (fPos.line >= lines || (fPos.line == ePos.line && fPos.character == ePos.character)) return ""
    if (fPos.line == ePos.line) contentLines(line).slice(fPos.character, ePos.character)
    else contentLines(fPos.line).slice(fPos.character, lineLength(fPos.line)) ++
      "\n" ++
      getLines(fPos.line + 1, ePos.line) ++
      contentLines(ePos.line).slice(0, ePos.character)
  }

  def getContentUpto(pos: Position) : String = {
    getContentBetween(Position(0, 0), pos)
  }

  def getContentFrom(pos: Position) : String = {
    getContentBetween(pos, Position(lines - 1, lineLength(lines - 1)))
  }

  def updatePasteBuffer(str : Option[String]) : Unit = {
    str match {
      case Some(s) => Viw.pasteBuffer = Some(s)
      case _ =>
    }
  }
}

case class RepeatCommand(state: State) extends Command(state) {
  def eval: Option[State] = {
    Viw.history.reverse.find(e => e._1.isInstanceOf[ModifyTextCommand]) match {
      case Some(entry) =>
        entry._2.filter(c => c != ".").foldLeft(Option(state))
          {(s : Option[State], k : String) => Viw.handleKey(k, s.getOrElse(state), true)}
      case _ => Some(state)
    }
  }
}

case class PasteCommand(state: State) extends Command(state) {
  def eval: Option[State] = Viw.pasteBuffer match {
    case Some(s) =>
      val strVec = s.split('\n').toVector
      val newLinePos = line + strVec.length - 1
      val newCharPos = if (strVec.length > 1) strVec.last.length - 1 else char + strVec.last.length
      Some(state.copy(content =
        getContentUpto(Position(line, char + 1)) ++
        s ++
        getContentFrom(Position(line, char + 1)),
      position = Position(newLinePos, newCharPos)))
    case None => Some(state)
  }
}

case class PasteBehindCommand(state: State) extends Command(state) {
  def eval: Option[State] = Viw.pasteBuffer match {
    case Some(s) => Some(state.copy(content =
      getContentUpto(position) ++
        s ++
        getContentFrom(position)))
    case None => Some(state)
  }
}