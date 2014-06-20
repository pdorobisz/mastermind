package pdorobisz.mastermind.commandline

import pdorobisz.mastermind.core._
import pdorobisz.mastermind.core.Incorrect
import pdorobisz.mastermind.core.IllegalArguments
import scala.annotation.tailrec

object Runner {

  def main(args: Array[String]) {
    val config: GameConfig = GameConfig(4, 6, uniqueColors = false, None).get
    val game = Game(config)
    play(game)
  }

  private def readColors() = readLine("colors: ").toList

  private def printResult(turn: Int, text: String) = println(s"#$turn: $text")

  @tailrec
  private def play(game: Game): Unit = {
    val input = readColors()
    game.guess(input) match {
      case Finished(turn: Int) =>
        printResult(turn, "game is already finished!")
      case GameOver(turn: Int) =>
        printResult(turn, "GAME OVER")
      case IllegalArguments(turn: Int) => {
        printResult(turn, "incorrect input")
        play(game)
      }
      case Incorrect(turn: Int, posOk: Int, colorOk: Int) => {
        printResult(turn, s"not guessed ($posOk/$colorOk)")
        play(game)
      }
      case Success(turn: Int) =>
        printResult(turn, "SUCCESS")
    }
  }
}
