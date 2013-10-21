package pdorobisz.mastermind

import org.scalatest.{FlatSpec, GivenWhenThen}
import org.scalatest.prop.TableDrivenPropertyChecks


class GameSpec extends FlatSpec with GivenWhenThen with TableDrivenPropertyChecks {

  private val TURN = 5

  "Game" should "be correctly initialized" in {
    Given("configuration")
    val config = GameConfig(4, 5).get

    When("game is initialized with this configuration")
    val game = Game.init(config)

    Then("new game should be created")
    assert(config.length === game.colors.size)
    assert(game.colors.max <= 'A' + config.numberOfColors)
    assert(0 === game.turn)
  }

  it should "return correct result when successful guess" in {
    Given("initialized game")
    val colors = List('A', 'B', 'A', 'C', 'C', 'D', 'A')
    val game = Game(colors, createConfigFromColors(colors), TURN)

    When("colors are guessed")
    val result = game.guess(colors)

    Then("success result should be returned")
    assert(Success(TURN + 1) === result)
  }

  it should "ignore characters case" in {
    Given("initialized game")
    val colors = List('A', 'B', 'C', 'D')
    val game = Game(colors, createConfigFromColors(colors), TURN)

    When("some colors but with different case are passed")
    val result = game.guess(colors.map(_.toLower))

    Then("success result should be returned")
    assert(Success(TURN + 1) === result)
  }

  it should "increment turn number after guess" in {
    Given("initialized game")
    val colors = List('A', 'B', 'C', 'D')
    val repeat = 3
    val game = Game(colors, createConfigFromColors(colors), TURN)

    When("colors are guessed few times")
    val wrongColors = List('A', 'A', 'A', 'A')
    1 to repeat foreach (_ => game.guess(wrongColors))

    Then("turn number should be increased by number of guesses")
    assert(TURN + repeat === game.turn)
  }

  it should "return correct result when invalid colors are passed" in {
    Given("initialized game")
    val config = GameConfig(4, 6).get
    val game = Game.init(config)

    When("invalid colors are passed")
    val invalidGuess = Seq.fill(config.length)(('A' + config.numberOfColors).toChar)
    val result = game.guess(invalidGuess)

    Then("IllegalArguments should be returned")
    assert(IllegalArguments(0) === result)
  }

  it should "return correct result when wrong number of colors is passed" in {
    Given("initialized game")
    val config = GameConfig(4, 6).get
    val game = Game.init(config)

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
        val config = GameConfig(colors.size, (colors.max max guess.max) + 1 - 'A').get
        assert(expected === Game(colors, config, TURN).guess(guess))
      }
  }

  private def createConfigFromColors(colors: Seq[Char]): GameConfig =
    GameConfig(colors.size, colors.max + 1 - GameConfig.FIRST_COLOR).get
}
