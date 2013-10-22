package pdorobisz.mastermind

import scala.util.Random


class Game private(_colors: Seq[Char], val config: GameConfig, private var _turn: Int) {

  val colors = _colors.map(_.toUpper)
  private var finished = false

  def guess(guessColors: Seq[Char]): Answer = {
    val guessColors1 = guessColors.map(_.toUpper)

    if (finished) {
      return Finished(_turn)
    } else if (!validateColors(guessColors1) || !validateLength(guessColors1)) {
      return IllegalArguments(_turn)
    }

    _turn += 1
    val (remainingColors, remainingGuessColors) = colors.zip(guessColors1).filter { case (x, y) => x != y }.unzip

    if (remainingGuessColors.size == 0) {
      finished = true
      Success(_turn)
    } else if (_turn != GameConfig.NO_GUESS_LIMIT && _turn == config.guessLimit) {
      finished = true
      GameOver(_turn)
    } else {
      val posOk: Int = guessColors1.size - remainingGuessColors.size
      val colorOk: Int = remainingColors.size - remainingColors.diff(remainingGuessColors).size
      Incorrect(_turn, posOk, colorOk)
    }
  }

  def turn = _turn

  private def validateColors(guessColors: Seq[Char]): Boolean =
    guessColors.find(_ > GameConfig.FIRST_COLOR + config.numberOfColors - 1).isEmpty

  private def validateLength(guessColors: Seq[Char]): Boolean = guessColors.size == colors.size
}

object Game {

  def apply(colors: Seq[Char], config: GameConfig, turn: Int): Game = new Game(colors, config, turn)

  def init(config: GameConfig): Game = new Game(generateColors(config), config, 0)

  private def generateColors(config: GameConfig): Seq[Char] =
    Seq.fill(config.length)((GameConfig.FIRST_COLOR + Random.nextInt(config.numberOfColors)).toChar)
}