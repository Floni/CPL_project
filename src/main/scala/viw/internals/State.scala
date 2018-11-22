package viw.internals

/**
  * State that represents the current contents of the text editor.
  */
trait StateFields {

  /**
    * @return all text in the buffer
    */
  def content: String

  /**
    * @return position of the cursor
    */
  def position: State.Position

  /**
    * @return region that is selected in content, start + end position
    */
  def selection: Option[(State.Position, State.Position)]

  /**
    * @return viw-mode toggle
    */
  def mode: Boolean

}

case class State(content: String,
                 position: State.Position,
                 selection: Option[(State.Position, State.Position)],
                 mode: Boolean)
    extends StateFields {
  val contentLines = content.split('\n').toVector
}

object State {
  def fromCursoredText(str: String, mode: Boolean = true): State = {
    val lines = str.split('\n')
    val characterPositions = lines.map { line =>
      "#.*#".r.findFirstMatchIn(line).map { m =>
        m.start
      }
    }
    val positions = characterPositions.zipWithIndex.collect {
      case (Some(c), line) => Position(line, c)
    }

    val content =
      "#.*#".r.replaceAllIn(str.replace("##", ""), m => m.matched(1).toString)
    val position = positions.headOption.getOrElse(Position(0, 0))

    State(content, position, None, mode)
  }

  /**
    * Position of the cursor
    * @param line line'th line, starts from 0
    * @param character character'th character, starts from 0
    */
  case class Position(line: Int, character: Int)
}
