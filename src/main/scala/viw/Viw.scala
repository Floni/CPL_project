package viw

import viw.internals.State
import viw.internals.State.Position

import scala.collection.mutable.ListBuffer

object Viw {
  var history : ListBuffer[State => Command] = new ListBuffer[State => Command]()
  var suspended : ListBuffer[SuspendableCommand] = new ListBuffer[SuspendableCommand]()
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
    "d" -> DeleteMovementCommand,
    "c" -> ChangeMovementCommand,
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
      command(state).eval match {
        case None =>
          if (suspended.nonEmpty && suspended.head == command(state)) {
            val suspendedCmd = suspended.head
            suspended.remove(0)
            return suspendedCmd.wake(command(state))
          }
          else if (command(state).isInstanceOf[SuspendableCommand]) suspended += command(state).asInstanceOf[SuspendableCommand]
          Some(state)
        case Some(s) =>
          // TODO: put this in recursive function for multiple suspended commands
          if (suspended.nonEmpty) {
            val suspendedCmd = suspended.head
            suspended.remove(0)
            suspendedCmd.wake(command(state))
          }
          else Some(s)
      }
    } else {
      Some(state)
    }
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