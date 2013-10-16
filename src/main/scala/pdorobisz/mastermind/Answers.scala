package pdorobisz.mastermind

sealed trait Answer

case class Success(turn: Int) extends Answer

case class Incorrect(turn: Int, posOk: Int, colorOk: Int) extends Answer