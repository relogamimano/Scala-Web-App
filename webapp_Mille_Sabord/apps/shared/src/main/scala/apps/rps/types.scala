package apps.rps

import cs214.webapp.UserId
import Hand.*

/** Hands are simply strings in this version of the game (use Emoji!) */
type Hand = String

object Hand:
  val Rock = "✊"
  val Paper = "✋"
  val Scissors = "✌️"
  val allHands = Set(Rock, Paper, Scissors)

extension (hand: Hand)
  def scoreAgainst(other: Hand): Int = (hand, other) match
    case (Rock, Scissors) | (Paper, Rock) | (Scissors, Paper) => 1
    case (Scissors, Rock) | (Rock, Paper) | (Paper, Scissors) => -1
    case _                                                    => 0

/** A view of the rock paper scissor's state for a specific client.
  *
  * The UI alternates between two views: selecting next hand and viewing the
  * results, attributing corresponding scores.
  *
  * @param phaseView
  *   A projection of the current phase of the game.
  * @param scoresView
  *   The score of each player.
  */
case class View(
    phaseView: PhaseView,
    scoresView: ScoresView
)

enum PhaseView:
  /** Players are selecting their next hand. */
  case SelectingHand(ready: Map[UserId, Boolean])

  /** Players are looking at each other's hand. */
  case ViewingHands(hands: Map[UserId, Hand])

type ScoresView = Map[UserId, Int]

enum Event:
  /** A player has chosen their hand. */
  case HandSelected(hand: Hand)

// ----------- Do we need to use type State ? ----------
//type State = Unit
enum Phase:
  case SelectingHand
  case ViewingHands
  case Done

case class State (  
  players: Vector[UserId],
  phase: Phase,
  versusHands : Map[UserId, Hand],
  score: Map[UserId, Int],
  round: Int
)