package pdorobisz.mastermind

import org.scalatest.{GivenWhenThen, FlatSpec}


class GameConfigSpec extends FlatSpec with GivenWhenThen {

  val VALID_LENGTH = (GameConfig.MAX_LENGTH + GameConfig.MIN_LENGTH) / 2
  val VALID_NUMBER_OF_COLORS = (GameConfig.MAX_NUMBER_OF_COLORS + GameConfig.MIN_NUMBER_OF_COLORS) / 2


  "Game configuration" should "be created when correct parameters are given" in {
    Given("valid parameters")
    When("configuration is created")
    val config = GameConfig(VALID_LENGTH, VALID_NUMBER_OF_COLORS).get

    Then("new config object should be created")
    assert(VALID_LENGTH === config.length)
    assert(VALID_NUMBER_OF_COLORS === config.numberOfColors)
  }

  it should "validate length" in {
    Given("invalid length values")
    val invalidLength1 = GameConfig.MIN_LENGTH - 1
    val invalidLength2 = GameConfig.MAX_LENGTH + 1

    When("configuration is created")
    val config1 = GameConfig(invalidLength1, VALID_NUMBER_OF_COLORS)
    val config2 = GameConfig(invalidLength2, VALID_NUMBER_OF_COLORS)

    Then("configuration shouldn't be created")
    assert(None === config1)
    assert(None === config2)
  }

  it should "validate number of colors" in {
    Given("invalid numbers of colors")
    val invalidNumberOfColors1 = GameConfig.MIN_NUMBER_OF_COLORS - 1
    val invalidNumberOfColors2 = GameConfig.MAX_NUMBER_OF_COLORS + 1

    When("configuration is created")
    val config1 = GameConfig(VALID_LENGTH, invalidNumberOfColors1)
    val config2 = GameConfig(VALID_LENGTH, invalidNumberOfColors2)

    Then("configuration shouldn't be created")
    assert(None === config1)
    assert(None === config2)
  }
}
