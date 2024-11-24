package apps
package rps

import cs214.webapp.*
import cs214.webapp.DecodingException

import scala.util.{Failure, Success, Try}

object Wire extends AppWire[Event, View]:
  import Event.*
  import View.*
  import ujson.*

  override object eventFormat extends WireFormat[Event]:
    extension(self: Hand)
      def tag = 
        self match 
          case Hand.Rock => "Rock"
          case Hand.Paper => "Paper"
          case Hand.Scissors => "Scissors"
          //case _ => "allHands"

    override def encode(event: Event): Value =
      event match 
        case HandSelected(hand) =>
          Str(hand.tag)
        

    override def decode(json: Value): Try[Event] =
      Try{
        val json_val = json.str
        json_val match
          case "Rock" => HandSelected(Hand.Rock)
          case "Paper" => HandSelected(Hand.Paper)
          case "Scissors" => HandSelected(Hand.Scissors)
          //case "allHands" => HandSelected(Hand.allHands)
      }
      

  override object viewFormat extends WireFormat[View]:
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

   

    override def encode(v: View): Value =
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


    override def decode(json: Value): Try[View] =
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



