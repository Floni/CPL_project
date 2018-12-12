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

  def getLines(start: Int, until: Int) : String = {
    if (start >= lines || until > lines || start == until) ""
    else contentLines.slice(start, until).mkString("\n") ++ "\n"
  }
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
    case Some(s) => {
      val strVec = s.split('\n').toVector
      val newLinePos = line + strVec.length - 1
      val newCharPos = if (strVec.length > 1) strVec.last.length - 1 else char + strVec.last.length
      Some(state.copy(content =
        getLines(0, line) ++
          contentLines(line).slice(0, char + 1) ++
          s ++
          (if (lineLength(line) > char + 1) contentLines(line).slice(char + 1, lineLength(line)) else "") ++
          (if (line < lines - 1) "\n" else "") ++
          getLines(line + 1, lines),
        position = Position(newLinePos, newCharPos)))
    }
    case None => Some(state)
  }
}

case class PasteBehindCommand(state: State) extends Command(state) {
  def eval: Option[State] = Viw.pasteBuffer match {
    case Some(s) => Some(state.copy(content =
      getLines(0, line) ++
        contentLines(line).slice(0, char) ++
        s ++
        (if (lineLength(line) > char) contentLines(line).slice(char, lineLength(line)) else "") ++
        (if (line < lines - 1) "\n" else "") ++
        getLines(line + 1, lines)))
    case None => Some(state)
  }
}