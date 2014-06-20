package pdorobisz.mastermind

import org.scalatest.{GivenWhenThen, FlatSpec}
import pdorobisz.mastermind.core.GameConfig


class GameConfigSpec extends FlatSpec with GivenWhenThen {

  val VALID_LENGTH = (GameConfig.MAX_LENGTH + GameConfig.MIN_LENGTH) / 2
  val VALID_NUMBER_OF_COLORS = (GameConfig.MAX_NUMBER_OF_COLORS + GameConfig.MIN_NUMBER_OF_COLORS) / 2
  val VALID_GUESS_LIMIT = GameConfig.MIN_GUESS_LIMIT


  "Game configuration" should "be created when correct parameters are given" in {
    Given("valid parameters")
    When("configuration is created")
    val config = GameConfig(VALID_LENGTH, VALID_NUMBER_OF_COLORS, uniqueColors = false, Some(VALID_GUESS_LIMIT)).get

    Then("new config object should be created")
    assert(VALID_LENGTH === config.length)
    assert(VALID_NUMBER_OF_COLORS === config.numberOfColors)
    assert(Some(VALID_GUESS_LIMIT) === config.guessLimit)
  }

  it should "validate length" in {
    Given("invalid length values")
    val invalidLength1 = GameConfig.MIN_LENGTH - 1
    val invalidLength2 = GameConfig.MAX_LENGTH + 1

    When("configuration is created")
    val config1 = GameConfig(invalidLength1, VALID_NUMBER_OF_COLORS, uniqueColors = false, Some(VALID_GUESS_LIMIT))
    val config2 = GameConfig(invalidLength2, VALID_NUMBER_OF_COLORS, uniqueColors = false, Some(VALID_GUESS_LIMIT))

    Then("configuration shouldn't be created")
    assert(None === config1)
    assert(None === config2)
  }

  it should "validate number of colors" in {
    Given("invalid numbers of colors")
    val invalidNumberOfColors1 = GameConfig.MIN_NUMBER_OF_COLORS - 1
    val invalidNumberOfColors2 = GameConfig.MAX_NUMBER_OF_COLORS + 1

    When("configuration is created")
    val config1 = GameConfig(VALID_LENGTH, invalidNumberOfColors1, uniqueColors = false, Some(VALID_GUESS_LIMIT))
    val config2 = GameConfig(VALID_LENGTH, invalidNumberOfColors2, uniqueColors = false, Some(VALID_GUESS_LIMIT))

    Then("configuration shouldn't be created")
    assert(None === config1)
    assert(None === config2)
  }

  it should "validate guess limit" in {
    Given("invalid guess limit")
    val invalidGuessLimit = Some(GameConfig.MIN_GUESS_LIMIT - 1)

    When("configuration is created")
    val config = GameConfig(VALID_LENGTH, VALID_NUMBER_OF_COLORS, uniqueColors = false, invalidGuessLimit)

    Then("configuration shouldn't be created")
    assert(None === config)
  }

  it should "validate unique colors" in {
    Given("length that exceeds available number of colors")
    val numberOfColors = VALID_LENGTH
    val length = numberOfColors + 1

    When("configuration with uniqueColors set to true is created")
    val config = GameConfig(length, numberOfColors, uniqueColors = true, Some(VALID_GUESS_LIMIT))

    Then("configuration shouldn't be created")
    assert(None === config)
  }
}
