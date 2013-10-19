package pdorobisz.mastermind

import scala.util.Random


class Game(val colors: Seq[Char], val config: GameConfig, val turn: Int) {

  def guess(guessColors: Seq[Char]): Answer = {
    if (guessColors.find(_ > 'A' + config.numberOfColors - 1).nonEmpty) {
      return IllegalArguments(turn)
    }

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

  def apply(colors: Seq[Char], config: GameConfig, turn: Int): Game = new Game(colors, config, turn)

  def init(config: GameConfig): Game = new Game(generateColors(config), config, 0)

  private def generateColors(config: GameConfig): Seq[Char] = {
    Seq.fill(config.length)(('A' + Random.nextInt(config.numberOfColors)).toChar)
  }
}