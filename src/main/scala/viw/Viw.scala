package viw

import viw.internals.State

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
    "P" -> PasteBehindCommand,
    "y" -> YankCommand,
    "1" -> CountCommand(1),
    "2" -> CountCommand(2),
    "3" -> CountCommand(3),
    "4" -> CountCommand(4),
    "5" -> CountCommand(5),
    "6" -> CountCommand(6),
    "7" -> CountCommand(7),
    "8" -> CountCommand(8),
    "9" -> CountCommand(9)
  )

  def processKey(key: String, state: State) : Option[State] = {
    if (commandMap.contains(key)) {
      val command = commandMap(key)
      history += command
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