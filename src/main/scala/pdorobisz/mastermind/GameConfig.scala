package pdorobisz.mastermind

class GameConfig private(val length: Int, val numberOfColors: Int)

object GameConfig {

  /**
   * Creates game configuration object.
   *
   * @param length number of elements in sequence
   * @param numberOfColors number of colors which can occur in sequence
   * @return game configuration
   */
  def apply(length: Int, numberOfColors: Int): Option[GameConfig] =
    if (isBetween(length, MIN_LENGTH, MAX_LENGTH) &&
      isBetween(numberOfColors, MIN_NUMBER_OF_COLORS, MAX_NUMBER_OF_COLORS)
    ) {
      Some(new GameConfig(length, numberOfColors))
    } else {
      None
    }

  private def isBetween(value: Int, low: Int, high: Int): Boolean = value >= low && value <= high

  val FIRST_COLOR = 'A'
  val LAST_COLOR = 'Z'
  val MIN_LENGTH = 1
  val MAX_LENGTH = 10
  val MIN_NUMBER_OF_COLORS = 1
  val MAX_NUMBER_OF_COLORS = LAST_COLOR - FIRST_COLOR + 1
}