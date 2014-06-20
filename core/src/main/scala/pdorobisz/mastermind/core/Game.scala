package pdorobisz.mastermind.core

import scala.util.Random
import scala.collection.mutable


class Game private(_colors: Seq[Char], val config: GameConfig, private var _turn: Int) {

  val colorsToGuess = _colors map (_.toUpper)
  private var finished = false

  def guess(colors: Seq[Char]): Answer = {
    val guessColors = colors map (_.toUpper)

    if (finished)
      return Finished(_turn)
    else if (!validateColors(guessColors) || !validateLength(guessColors))
      return IllegalArguments(_turn)

    _turn += 1
    val (colorsNotGuessed, incorrectColors) = (colorsToGuess zip guessColors filter {
      case (x, y) => x != y
    }).unzip

    if (incorrectColors.size == 0) {
      finished = true
      Success(_turn)
    } else if (config.guessLimit.isDefined && _turn == config.guessLimit.get) {
      finished = true
      GameOver(_turn)
    } else {
      val posOk: Int = guessColors.size - incorrectColors.size
      val colorOk: Int = colorsNotGuessed.size - (colorsNotGuessed diff incorrectColors).size
      Incorrect(_turn, posOk, colorOk)
    }
  }

  def turn: Int = _turn

  private def validateColors(guessColors: Seq[Char]): Boolean =
    guessColors forall (_ <= GameConfig.FIRST_COLOR + config.numberOfColors - 1)

  private def validateLength(guessColors: Seq[Char]): Boolean = guessColors.size == colorsToGuess.size
}

object Game {

  /**
   * Initialized new game.
   *
   * @param config game config
   * @return game object
   */
  def apply(config: GameConfig): Game = new Game(generateColors(config), config, 0)


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

  private def validateTurn(turn: Int, guessLimit: Option[Int]) = turn >= 0 && (guessLimit.isEmpty || turn < guessLimit.get)

  private def validateColors(chars: Seq[Char], config: GameConfig): Boolean =
    chars.size == config.length &&
      (chars forall (c => c >= GameConfig.FIRST_COLOR && c - GameConfig.FIRST_COLOR < config.numberOfColors)) &&
      (!config.uniqueColors || chars.distinct.size == chars.size)

  private def generateColors(config: GameConfig): Seq[Char] = {
    val availableColors = (GameConfig.FIRST_COLOR until (GameConfig.FIRST_COLOR + config.numberOfColors).toChar).toBuffer
    Seq.fill(config.length)(getRandomColor(availableColors, config.uniqueColors))
  }

  private def getRandomColor(colors: mutable.Buffer[Char], remove: Boolean): Char = {
    val index = Random.nextInt(colors.size)
    if (remove) colors.remove(index) else colors(index)
  }
}