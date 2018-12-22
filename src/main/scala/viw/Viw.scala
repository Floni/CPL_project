package viw

import viw.internals.State

import scala.collection.mutable.ListBuffer

object Viw {
  // The history contains the resulting type of command and the key combinations that formed them
  var history : ListBuffer[(Command, ListBuffer[String])] = new ListBuffer[(Command, ListBuffer[String])]()
  // History that is being build at the moment
  var subHistory : ListBuffer[String] = new ListBuffer[String]()
  // List of all currently suspended commands
  var suspended : ListBuffer[SuspendableCommand] = new ListBuffer[SuspendableCommand]()
  var pasteBuffer : Option[String] = None

  // Maps input keys to the appropriate Command
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
    "9" -> CountCommand(9),
    ">" -> IndentIncCommand,
    "<" -> IndentDecCommand
  )

  // List of all commands that are repeatable, i.e. dd, cc, yy, >>, <<
  val repeatableCommands : ListBuffer[State => SuspendableCommand] =
    ListBuffer(DeleteMovementCommand, ChangeMovementCommand, YankCommand,
      IndentIncCommand, IndentDecCommand)

  def processKey(key: String, state: State) : Option[State] = {
    if (commandMap.contains(key)) {
      val command = commandMap(key)
      subHistory += key

      // Check whether last command was a count command
      val lastCmdCount = subHistory.length > 1 && subHistory(subHistory.length - 2).charAt(0).isDigit

      // Get the result of the command which in None for SuspendableCommands or if the last command was
      // a count command and the current key is a 0, this is required to use count commands that contain 0
      val result = if (command == StartLineCommand && lastCmdCount) None else command(state).eval
      result match {
        case None =>

          // Repeated command
          if (suspended.nonEmpty && suspended.last == command(state) && repeatableCommands.contains(command)) {
            val suspendedCmd = suspended.last
            suspended.clear()
            val suspResult = suspendedCmd.wake(command(state))
            history += ((suspResult, subHistory))
            subHistory = ListBuffer()
            return suspResult.eval
          }

          // Previous command was a count command
          if (lastCmdCount) {
            val suspendedCmd = suspended.last
            suspended.remove(suspended.length - 1)
            suspended.append(suspendedCmd.asInstanceOf[CountCommand].concatCountCommand(command(state)))
          }
          // Add command to suspended list
          else if (command(state).isInstanceOf[SuspendableCommand]) {
            suspended.append(command(state).asInstanceOf[SuspendableCommand])
          }
          return None

        case Some(_) =>
          // Command has a result
          if (suspended.nonEmpty) return foldSuspendedCommands(command(state))
      }
      history += ((command(state), subHistory))
      subHistory = ListBuffer()
      return result
    }
    None
  }

  // Recursive function that folds over the suspended commands
  // Passes the current (non-suspended) command to the wake function
  // of the last suspended command and then passes the result of that wake
  // to the next suspended command and so on
  def foldSuspendedCommands(command: Command): Option[State] = {
    val suspendedCmd = suspended.last
    suspended.remove(suspended.length - 1)
    if (suspended.nonEmpty) {
      foldSuspendedCommands(suspendedCmd.wake(command))
    } else {
      val resultCommand = suspendedCmd.wake(command)
      history += ((resultCommand, subHistory))
      subHistory = ListBuffer()
      resultCommand.eval
    }
  }
}