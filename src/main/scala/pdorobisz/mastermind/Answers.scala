package pdorobisz.mastermind

sealed trait Answer

case class Finished(turn: Int) extends Answer

case class GameOver(turn: Int) extends Answer

case class IllegalArguments(turn: Int) extends Answer

case class Incorrect(turn: Int, posOk: Int, colorOk: Int) extends Answer

case class Success(turn: Int) extends Answer
