package pdorobisz.mastermind

class GameConfig private(val length: Int, val numberOfColors: Int, val uniqueColors: Boolean, val guessLimit: Int) {
  override def toString() = s"[length: $length, numberOfColors: $numberOfColors, uniqueColors: $uniqueColors, guessLimit: $guessLimit]"
}

object GameConfig {

  /**
   * Creates game configuration object.
   *
   * @param length number of elements in sequence
   * @param numberOfColors number of colors which can occur in sequence
   * @param uniqueColors if colors shouldn't be repeated in sequence
   * @param guessLimit guess limit (maximum number of turns in which sequence should be guessed)
   * @return game configuration
   */
  def apply(length: Int, numberOfColors: Int, uniqueColors: Boolean, guessLimit: Int): Option[GameConfig] =
    if (validateLength(length) &&
      validateNumberOfColors(numberOfColors) &&
      validateGuessLimit(guessLimit) &&
      validateUniqueColors(numberOfColors, length, uniqueColors)
    ) {
      Some(new GameConfig(length, numberOfColors, uniqueColors, guessLimit))
    } else {
      None
    }

  def validateLength(length: Int): Boolean = length >= MIN_LENGTH && length <= MAX_LENGTH

  def validateNumberOfColors(numberOfColors: Int): Boolean = numberOfColors >= MIN_NUMBER_OF_COLORS && numberOfColors <= MAX_NUMBER_OF_COLORS

  def validateGuessLimit(guessLimit: Int): Boolean = guessLimit == NO_GUESS_LIMIT || guessLimit >= MIN_GUESS_LIMIT

  def validateUniqueColors(numberOfColors: Int, length: Int, unique: Boolean): Boolean = !unique || length <= numberOfColors

  val FIRST_COLOR = 'A'
  val LAST_COLOR = 'Z'
  val MIN_LENGTH = 1
  val MAX_LENGTH = 10
  val MIN_NUMBER_OF_COLORS = 1
  val MAX_NUMBER_OF_COLORS = LAST_COLOR - FIRST_COLOR + 1
  val NO_GUESS_LIMIT = 0
  val MIN_GUESS_LIMIT = 1
}