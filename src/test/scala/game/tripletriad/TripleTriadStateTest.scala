package game.tripletriad

import org.scalatest.{FlatSpec, Matchers}
import game.{BluePlayer, Pos, RedPlayer}

class TripleTriadStateTest extends FlatSpec with Matchers {

  import TripleTriadState._

  "The Universe" should "exists" in {
    true should be(true)
  }

  "A triple triad game" should "exists" in {
    val game = TripleTriadState(activePlayer = RedPlayer)
    game should not equal null
  }

  it should "allow to make a move" in {
    val game = TripleTriadState(activePlayer = RedPlayer)
        .move((RedPlayer, Card(1, 1, 1, 1), Pos(0, 0)))

    game.isSuccess should be(true)
  }

  it should "flip cards" in {
    val game = TripleTriadState(activePlayer = RedPlayer)
        .move((RedPlayer, Card(1, 1, 1, 1), Pos(0, 0)))
        .flatMap(_.move((BluePlayer, Card(2, 2, 2, 2), Pos(0, 1))))
    
    game.isSuccess should be(true)
  }
}
