package pdorobisz.mastermind

import org.scalatest.{FlatSpec, GivenWhenThen}
import org.scalatest.prop.TableDrivenPropertyChecks
import pdorobisz.mastermind.core._
import pdorobisz.mastermind.core.GameOver
import pdorobisz.mastermind.core.Finished
import pdorobisz.mastermind.core.IllegalArguments


class GameSpec extends FlatSpec with GivenWhenThen with TableDrivenPropertyChecks {

  private val TURN = 5
  private val GUESS_LIMIT = TURN * 2

  "Game" should "be correctly initialized" in {
    Given("configuration")
    val config = GameConfig(4, 5, uniqueColors = false, Some(GUESS_LIMIT)).get

    When("game is initialized with this configuration")
    val game = Game(config)

    Then("new game should be created")
    assert(config.length === game.colorsToGuess.size)
    assert(game.colorsToGuess.max <= 'A' + config.numberOfColors)
    assert(0 === game.turn)
  }

  it should "be initialized with unique colors" in {
    Given("game initialized with two colors and enabled color uniqueness")
    val config = GameConfig(2, 2, uniqueColors = true, None).get
    val game = Game(config)

    When("guesses with different colors are tried")
    val result1 = game.guess(List('A', 'B'))
    val result2 = game.guess(List('B', 'A'))

    Then("one of guesses is successful")
    val success = result1 == Success(1) || result2 == Success(2)
    assert(true === success)
  }

  it should "return correct result when successful guess" in {
    Given("initialized game")
    val colors = List('A', 'B', 'A', 'C', 'C', 'D', 'A')
    val game = Game.restore(colors, createConfigFromColors(colors), TURN).get

    When("colors are guessed")
    val result = game.guess(colors)

    Then("success result should be returned")
    assert(Success(TURN + 1) === result)
  }

  it should "ignore characters case" in {
    Given("initialized game")
    val colors = List('A', 'B', 'C', 'D')
    val game = Game.restore(colors, createConfigFromColors(colors), TURN).get

    When("some colors but with different case are passed")
    val result = game.guess(colors.map(_.toLower))

    Then("success result should be returned")
    assert(Success(TURN + 1) === result)
  }

  it should "increment turn number after guess" in {
    Given("initialized game")
    val colors = List('A', 'B', 'C', 'D')
    val repeat = 3
    val game = Game.restore(colors, createConfigFromColors(colors), TURN).get

    When("colors are guessed few times")
    val wrongColors = List('A', 'A', 'A', 'A')
    1 to repeat foreach (_ => game.guess(wrongColors))

    Then("turn number should be increased by number of guesses")
    assert(TURN + repeat === game.turn)
  }

  it should "finish game when maximum number of guesses reached" in {
    Given("initialized game with turn number set to guessLimit - 1")
    val colors = List('A', 'B', 'C', 'D')
    val game = Game.restore(colors, createConfigFromColors(colors), GUESS_LIMIT - 1).get

    When("incorrect colors are guessed")
    val wrongColors = List('A', 'A', 'A', 'A')
    val result = game.guess(wrongColors)

    Then("GameOver should be returned")
    assert(GameOver(GUESS_LIMIT) === result)
  }

  it should "not accept new guesses when game is finished with success" in {
    Given("game finished with success")
    val colors = List('A', 'B', 'C', 'D')
    val game = Game.restore(colors, createConfigFromColors(colors), TURN).get
    val result1 = game.guess(colors)

    When("colors are guessed after finishing game")
    val result2 = game.guess(colors)

    Then("Finished should be returned")
    assert(Success(TURN + 1) === result1)
    assert(Finished(TURN + 1) === result2)
  }

  it should "not accept new guesses when game is lost" in {
    Given("lost game")
    val colors = List('A', 'B', 'C', 'D')
    val wrongColors = List('A', 'A', 'A', 'A')
    val game = Game.restore(colors, createConfigFromColors(colors), GUESS_LIMIT - 1).get
    val result1 = game.guess(wrongColors)

    When("colors are guessed after finishing game")
    val result2 = game.guess(colors)

    Then("Finished should be returned")
    assert(GameOver(GUESS_LIMIT) === result1)
    assert(Finished(GUESS_LIMIT) === result2)
  }

  it should "return correct result when invalid colors are passed" in {
    Given("initialized game")
    val config = GameConfig(4, 6, uniqueColors = false, Some(GUESS_LIMIT)).get
    val game = Game(config)

    When("invalid colors are passed")
    val invalidGuess = Seq.fill(config.length)(('A' + config.numberOfColors).toChar)
    val result = game.guess(invalidGuess)

    Then("IllegalArguments should be returned")
    assert(IllegalArguments(0) === result)
  }

  it should "return correct result when wrong number of colors is passed" in {
    Given("initialized game")
    val config = GameConfig(4, 6, uniqueColors = false, Some(GUESS_LIMIT)).get
    val game = Game(config)

    When("too many colors are passed")
    val result = game.guess(Seq.fill(config.length + 1)('A'))

    Then("IllegalArguments should be returned")
    assert(IllegalArguments(0) === result)
  }

  val incorrectGuesses = Table(
    ("colors", "guess", "expected"),
    (List('A', 'B', 'A'), List('A', 'A', 'A'), Incorrect(TURN + 1, 2, 0)),
    (List('A', 'B', 'A'), List('B', 'A', 'B'), Incorrect(TURN + 1, 0, 2)),
    (List('A', 'B', 'B', 'C'), List('A', 'B', 'C', 'B'), Incorrect(TURN + 1, 2, 2)),
    (List('A', 'B', 'B', 'C'), List('A', 'D', 'C', 'B'), Incorrect(TURN + 1, 1, 2)),
    (List('A', 'B', 'C', 'D'), List('B', 'E', 'B', 'E'), Incorrect(TURN + 1, 0, 1)),
    (List('A', 'B', 'C', 'B', 'D'), List('B', 'C', 'B', 'D', 'A'), Incorrect(TURN + 1, 0, 5)),
    (List('A', 'B', 'C', 'D'), List('E', 'E', 'E', 'E'), Incorrect(TURN + 1, 0, 0))
  )

  forAll(incorrectGuesses) {
    (colors: Seq[Char], guess: Seq[Char], expected: Answer) =>
      it should s"return $expected for $colors when guess is $guess" in {
        val config = GameConfig(colors.size, (colors.max max guess.max) + 1 - 'A', uniqueColors = false, Some(GUESS_LIMIT)).get
        assert(expected === Game.restore(colors, config, TURN).get.guess(guess))
      }
  }

  val incorrectRestoreParameters = Table(
    ("colors", "config", "turn"),
    // incorrect guess limit
    (List('A', 'B', 'C', 'D'), GameConfig(4, 6, uniqueColors = false, Some(GUESS_LIMIT)).get, -1),
    (List('A', 'B', 'C', 'D'), GameConfig(4, 6, uniqueColors = false, Some(GUESS_LIMIT)).get, GUESS_LIMIT),
    (List('A', 'B', 'C', 'D'), GameConfig(4, 6, uniqueColors = false, Some(GUESS_LIMIT)).get, GUESS_LIMIT + 1),

    // incorrect colors
    (List('A', 'B', 'C', 'D'), GameConfig(3, 6, uniqueColors = false, Some(GUESS_LIMIT)).get, TURN),
    (List('A', 'B', 'C', 'D'), GameConfig(4, 3, uniqueColors = false, Some(GUESS_LIMIT)).get, TURN),

    // colors not unique
    (List('A', 'B', 'B', 'A'), GameConfig(4, 4, uniqueColors = true, Some(GUESS_LIMIT)).get, TURN)
  )

  forAll(incorrectRestoreParameters) {
    (colors: Seq[Char], config: GameConfig, turn: Int) =>
      it should s"not restore game for invalid parameters: colors: $colors, config: $config, turn: $turn" in {
        assert(None === Game.restore(colors, config, turn))
      }
  }

  private def createConfigFromColors(colors: Seq[Char]): GameConfig =
    GameConfig(colors.size, colors.max + 1 - GameConfig.FIRST_COLOR, uniqueColors = false, Some(GUESS_LIMIT)).get
}
