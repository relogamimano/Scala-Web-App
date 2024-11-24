import ujson.*
import scala.util.{Failure, Success, Try}
import io.undertow.util.FlexBase64.Decoder

type Hand = String
type UserId = String

import Hand.*

/** Hands are simply strings in this version of the game (use Emoji!) */

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

case class State (
  players: Vector[UserId],
  phase: Phase,
  score: Map[UserId, Int],
)

extension(self: Hand)
      def tag = 
        self match 
          case Hand.Rock => "Rock"
          case Hand.Paper => "Paper"
          case Hand.Scissors => "Scissors"
          //case _ => "allHands"
    extension(self: PhaseView)
      def tag = 
        self match 
          case PhaseView.SelectingHand(ready) => "Selecting"
          case PhaseView.ViewingHands(hands) => "Viewing"

   

    def encode(v: View): Value =
        v match 
          case View(phaseView,scoresView) => 
            Arr(
                Obj(
                    "tag" -> phaseView.tag,
                    "value" -> {
                      phaseView match 
                      case PhaseView.SelectingHand(ready) => 
                        Arr(ready.map((key,value) => Obj("key" -> key, "value" -> Bool(value))))
                      case PhaseView.ViewingHands(hands) => 
                        Arr(hands.map((key,value) => Obj("key" -> key, "value" -> Str(value.tag))))
                    }
                  ),
                Arr(scoresView.map((key,value) => Obj("key" -> key, "value" -> Num(value))))
            )
      
    def decodeHand(hand: String): Hand = 
      hand match 
        case "Rock" => Hand.Rock
        case "Paper" => Hand.Paper
        case "Scissors" => Hand.Scissors


    def decode(json: Value): Try[View] =
      Try{
        val json_val = json.arr
        val (phase,score) = (json_val(0).obj, json_val(1).arr)
        val phase_val = phase("value").arr.toList.flatMap(_.arr.toList)
        val score_val = score.toList.flatMap(_.arr.toList)
        val phase_view = 
          phase("tag").str match 
          case "Selecting" => 
            PhaseView.SelectingHand(phase_val.map(x => Map(x.obj("key").str -> x.obj("value").bool)).flatten.toMap)
          case "Viewing" => 
            PhaseView.ViewingHands(phase_val.map(x => Map(x.obj("key").str -> decodeHand(x.obj("value").str))).flatten.toMap)
        val score_view : ScoresView= 
          (score_val.map(x => Map(x.obj("key").str -> x.obj("value").num.toInt)).flatten.toMap)
        View(phase_view,score_view)
      }
val rock : Hand = Hand.Rock
val scissor : Hand = Hand.Scissors
//val phase_View = PhaseView.SelectingHand(Map("1" -> true, "2" -> false))
val phase_View = PhaseView.ViewingHands(Map("1"-> rock, "2" -> scissor))
val scores_View = Map("1" -> 2, "2" -> 1)
val view = View(phase_View, scores_View)

val encoded = encode(view)



val json_val = encoded.arr
val (phase,score) = (json_val(0).obj, json_val(1).arr)
val phase_val = phase("value").arr.toList.flatMap(_.arr.toList)
val score_val = score.toList.flatMap(_.arr.toList)
val phase_view = 
    phase("tag").str match 
    case "Selecting" => 
      PhaseView.SelectingHand(phase_val.map(x => Map(x.obj("key").str -> x.obj("value").bool)).flatten.toMap)
    case "Viewing" => 
      PhaseView.ViewingHands(phase_val.map(x => Map(x.obj("key").str -> decodeHand(x.obj("value").str))).flatten.toMap)
val score_view : ScoresView= 
          (score_val.map(x => Map(x.obj("key").str -> x.obj("value").num.toInt)).flatten.toMap)
View(phase_view,score_view)

decode(encode(view))
view
