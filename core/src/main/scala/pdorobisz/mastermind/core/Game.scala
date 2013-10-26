package pdorobisz.mastermind.core

import scala.util.Random
import scala.collection.mutable.Buffer


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

  /**
   * Initialized new game.
   *
   * @param config game config
   * @return game object
   */
  def init(config: GameConfig): Game = new Game(generateColors(config), config, 0)


  /**
   * Restores game state.
   *
   * @param colors colors to guess
   * @param config game config
   * @param turn current turn
   * @return game object
   */
  def restore(colors: Seq[Char], config: GameConfig, turn: Int): Option[Game] =
    if (validateTurn(turn, config.guessLimit) && validateColors(colors, config)) {
      Some(new Game(colors, config, turn))
    } else {
      None
    }

  private def validateTurn(turn: Int, guessLimit: Int) =
    turn >= 0 && (guessLimit == GameConfig.NO_GUESS_LIMIT || turn < guessLimit)

  private def validateColors(chars: Seq[Char], config: GameConfig): Boolean =
    chars.size == config.length &&
      chars.forall(c => c >= GameConfig.FIRST_COLOR && c - GameConfig.FIRST_COLOR < config.numberOfColors) &&
      (!config.uniqueColors || chars.distinct.size == chars.size)

  private def generateColors(config: GameConfig): Seq[Char] = {
    val availableColors = (GameConfig.FIRST_COLOR until (GameConfig.FIRST_COLOR + config.numberOfColors).toChar).toBuffer
    Seq.fill(config.length)(getRandomColor(availableColors, config.uniqueColors))
  }

  private def getRandomColor(colors: Buffer[Char], remove: Boolean): Char = {
    val index = Random.nextInt(colors.size)
    if (remove) {
      colors.remove(index)
    } else {
      colors(index)
    }
  }
}