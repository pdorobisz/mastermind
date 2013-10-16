package pdorobisz.mastermind


class Game(val colors: Seq[Char], val turn: Int) {

  def guess(guessColors: Seq[Char]): Answer = {
    val newTurn: Int = turn + 1
    val (remainingColors, remainingGuessColors) = colors.zip(guessColors).filter { case (x, y) => x != y }.unzip

    if (remainingGuessColors.size == 0) {
      Success(newTurn)
    } else {
      val posOk: Int = guessColors.size - remainingGuessColors.size
      val colorOk: Int = remainingColors.size - remainingColors.diff(remainingGuessColors).size
      Incorrect(newTurn, posOk, colorOk)
    }
  }
}

object Game {

  def apply(colors: Seq[Char], turn: Int): Game = new Game(colors: Seq[Char], turn: Int)

  def init(colors: Seq[Char]): Game = new Game(colors, 0)
}