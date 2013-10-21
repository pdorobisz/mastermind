package pdorobisz.mastermind

class GameConfig private(val length: Int, val numberOfColors: Int, val guessLimit: Int)

object GameConfig {

  /**
   * Creates game configuration object.
   *
   * @param length number of elements in sequence
   * @param numberOfColors number of colors which can occur in sequence
   * @return game configuration
   */
  def apply(length: Int, numberOfColors: Int, guessLimit: Int): Option[GameConfig] =
    if (validateLength(length) && validateNumberOfColors(numberOfColors) && validateGuessLimit(guessLimit)) {
      Some(new GameConfig(length, numberOfColors, guessLimit))
    } else {
      None
    }

  def validateLength(length: Int): Boolean = length >= MIN_LENGTH && length <= MAX_LENGTH

  def validateNumberOfColors(numberOfColors: Int): Boolean = numberOfColors >= MIN_NUMBER_OF_COLORS && numberOfColors <= MAX_NUMBER_OF_COLORS

  def validateGuessLimit(guessLimit: Int): Boolean = guessLimit == NO_GUESS_LIMIT || guessLimit >= MIN_GUESS_LIMIT

  val FIRST_COLOR = 'A'
  val LAST_COLOR = 'Z'
  val MIN_LENGTH = 1
  val MAX_LENGTH = 10
  val MIN_NUMBER_OF_COLORS = 1
  val MAX_NUMBER_OF_COLORS = LAST_COLOR - FIRST_COLOR + 1
  val NO_GUESS_LIMIT = 0
  val MIN_GUESS_LIMIT = 1
}