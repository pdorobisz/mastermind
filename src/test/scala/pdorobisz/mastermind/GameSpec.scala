package pdorobisz.mastermind

import org.scalatest.{FlatSpec, GivenWhenThen}
import org.scalatest.prop.TableDrivenPropertyChecks


class GameSpec extends FlatSpec with GivenWhenThen with TableDrivenPropertyChecks {

  private val TURN = 5

  "Game" should "be correctly initialized" in {
    Given("configuration")
    val config = GameConfig(4, 5)

    When("game is initialized with this configuration")
    val game = Game.init(config)

    Then("new game should be created")
    assert(config.length === game.colors.size)
    assert(game.colors.max <= 'A' + config.numberOfColors)
    assert(0 === game.turn)
  }

  it should "return correct result when successful guess" in {
    Given("initialized game")
    val colors = List('a', 'b', 'a', 'c', 'c', 'd', 'a')
    val game = Game(colors, createConfigFromColors(colors), TURN)

    When("colors are guessed")
    val result = game.guess(colors)

    Then("success result should be returned")
    assert(Success(TURN + 1) === result)
  }

  val incorrectGuesses = Table(
    ("colors", "guess", "expected"),
    (List('a', 'b', 'a'), List('a', 'a', 'a'), Incorrect(TURN + 1, 2, 0)),
    (List('a', 'b', 'a'), List('b', 'a', 'b'), Incorrect(TURN + 1, 0, 2)),
    (List('a', 'b', 'b', 'c'), List('a', 'b', 'c', 'b'), Incorrect(TURN + 1, 2, 2)),
    (List('a', 'b', 'b', 'c'), List('a', 'd', 'c', 'b'), Incorrect(TURN + 1, 1, 2)),
    (List('a', 'b', 'c', 'd'), List('b', 'e', 'b', 'e'), Incorrect(TURN + 1, 0, 1)),
    (List('a', 'b', 'c', 'b', 'd'), List('b', 'c', 'b', 'd', 'a'), Incorrect(TURN + 1, 0, 5)),
    (List('a', 'b', 'c', 'd'), List('e', 'e', 'e', 'e'), Incorrect(TURN + 1, 0, 0))
  )

  forAll(incorrectGuesses) {
    (colors: Seq[Char], guess: Seq[Char], expected: Answer) =>
      it should s"return $expected for $colors when guess is $guess" in {
        val config: GameConfig = createConfigFromColors(colors)
        assert(expected === Game(colors, config, TURN).guess(guess))
      }
  }

  private def createConfigFromColors(colors: Seq[Char]): GameConfig = GameConfig(colors.size, colors.max - 'A')
}
