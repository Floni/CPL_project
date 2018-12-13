package viw

import viw.internals.State

import scala.collection.mutable.ListBuffer

object Viw {
  var history : ListBuffer[(Command, ListBuffer[String])] = new ListBuffer[(Command, ListBuffer[String])]()
  var subHistory : ListBuffer[String] = new ListBuffer[String]()
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
    "9" -> CountCommand(9),
    ">" -> IndentIncCommand,
    "<" -> IndentDecCommand
  )

  val repeatableCommand = ListBuffer(DeleteMovementCommand, ChangeMovementCommand, IndentIncCommand, IndentDecCommand)

  def processKey(key: String, state: State) : Option[State] = {
    if (commandMap.contains(key)) {
      val command = commandMap(key)
      subHistory += key

      val result = command(state).eval
      result match {
        case None =>
          if (suspended.nonEmpty && suspended.last == command(state) && repeatableCommand.contains(command)) {
            val suspendedCmd = suspended.last
            suspended.clear()
            val suspResult = suspendedCmd.wake(command(state))
            history += ((suspResult, subHistory))
            subHistory = ListBuffer()
            return suspResult.eval
          }
          if (command(state).isInstanceOf[SuspendableCommand]) {
            suspended.append(command(state).asInstanceOf[SuspendableCommand])
          }
          return Some(state)
        case Some(_) =>
          if (suspended.nonEmpty) return foldSuspendedCommands(command(state))
      }
      history += ((command(state), subHistory))
      subHistory = ListBuffer()
      return result
    }
    Some(state)
  }

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