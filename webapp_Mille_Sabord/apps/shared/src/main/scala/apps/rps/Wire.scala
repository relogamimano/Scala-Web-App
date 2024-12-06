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
    override def encode(event: Event): Value =
      event match 
        case DiceClicked(diceId) => Obj("type" -> "DiceClicked", "diceId" -> diceId)
        case ButtonClicked(buttonId) => Obj("type" -> "ButtonClicked", "buttonId" -> buttonId)        

    override def decode(json: Value): Try[Event] = Try:
      js("type").str match
        case "DiceClicked" => DiceClicked(js("diceId").num.toInt)
        case "ButtonClicked" => ButtonClicked(js("buttonId").num.toInt)
        case _ => throws DecodingException(f"Invalid memory event $js")
           

//========================== VIEW ENCODING HIERACHY ===============================
//
//                                         [View]
//                                        /      \
//                                       /        \
//                            [StateView]         [ScoresView]              
//                            /    |     \
//                           /     |      \
//                          /      |       \
//              [PhaseView]   [DiceView]  [ButtonView]         
//                                 |
//                               [DICE]
//
//============================= VIEW WIRE ===================================
    override object ViewFormat extends WireFormat[View]:
      val scoresmap = MapWire(UserId, int)
      override def encode(view: View): Value =
        Obj(
          "stateView" -> StateViewFormat.encode(view.stateView),
          "scoresView" -> scoresmap.encode(view.scoresView)
        )
      override def decode(js: Value): Try[View] = Try:
        View(
          StateViewFormat.decode(js("stateView")).get,
          scoresmap.decode(js("scoresView")).get
        )
//============================= STATE VIEW WIRE =================================
    override object StateViewFormat extends WireFormat[StateView]:

      override def encode(stateView: StateView): Value = 
        stateView match
          case Playing(phase, currentPlayer, diceView, button) =>
            Obj("type" -> "Playing",
            "phase" -> PhaseViewFormat.encode(phase),
            "currentPlayer" -> currentPlayer,
            "diceView" -> VectorWire(DiceViewFormat).encode(diceVew))
          case Finished(winnerId) =>
            Obj("type" -> "Finished",
            "winnerId" -> winnerId)

      override def decode(js: Value): Try[StateView] = Try:
        js("type").str match
          case "Playing" =>
            Playing(js("phase"), js("currentPlayer"), js("dice"), js("button"))
          case "Finished" =>
            Finished(js("winnerId"))
//================================ PHASE VIEW WIRE ==============================
    override object PhaseViewFormat extends WireFormat[PhaseView]:
      val readymap = MapWire(UserId, Boolean)
      val handsmap = MapWire(UserId, Hands)

      override def encode(phaseView: PhaseView): Value =
        phaseView match
          case SelectingHand(ready) =>
            Obj("type" -> "SelectingHand",
            "ready" -> readymap.encode(ready))
          case ViewingHands(hands) => 
            Obj("type" -> "ViewingHands",
            "hands" -> handsmap.encode(hands))
        
      override def decode(js: Value): Try[PhaseView] = Try:
        js("type").str match
          case "SelectingHand" => 
            SelectingHand(readymap.decode(js("ready")))
          case "ViewingHands" =>
            ViewingHands(handsmap.decode(js("hands")))
//=============================== BUTTON VIEW WIRE ================================
    override object ButtonViewFormat extends WireFormat[ButtonView]:
      override def encode(buttonView: ButtonView): Value =
        Obj("type" -> buttonView.value(), "button" -> buttonView.button)

      override def decode(js: Value): Try[ButtonView] = Try:
        ButtonView.valueOf(js("type").str)
//============================== DICE VIEW WIRE ====================================
    override object DiceViewFormat extends WireFormat[DiceView]:
      extension(self: Dice) // DICE ENCODING
        def tag = 
          self match 
            case Dice.Skull => "Skull"
            case Dice.Diamond => "Diamond"
            case Dice.Coin => "Coin"
            case Dice.Sword => "Sword"
            case Dice.Monkey => "Monkey"
            case Dice.Parrot => "Parrot"

      override def encode(diceView: DiceView): Value =
        diceView match
          case Selected(dice) =>
            Obj("type" -> "Selected", "dice" -> dice.tag)
          case Unselected(dice) =>
            Obj("type" -> "Unselected", "dice" -> dice.tag)
          case Skull(dice) =>
            Obj("type" -> "Skull", "dice" -> dice.tag)
      
      override def decode(js: Value): Try[DiceView] = Try:
        js("type").str match
          case "Selected" => Selected(js("dice"))
          case "Unselected" => Unselected(js("dice"))
          case "Skull" => Skull(js("dice"))    