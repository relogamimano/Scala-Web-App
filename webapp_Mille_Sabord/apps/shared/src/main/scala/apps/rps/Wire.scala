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
        case ButtonClicked(buttonId) => Obj("type" -> "ButtonClicked", "buttonId" -> ButtonIdFormat.encode(buttonId))        

    override def decode(json: Value): Try[Event] = Try:
      json("type").str match
        case "DiceClicked" => DiceClicked(json("diceId").num.toInt)
        case "ButtonClicked" => ButtonClicked(ButtonIdFormat.decode(json("buttonId")).get)
        case _ => throw DecodingException(f"Invalid memory event $json")
           

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
//                                 |            |
//                               [DICE]         [Button]
//
//============================= VIEW WIRE ===================================
    override object ViewFormat extends WireFormat[View]:
      val scoresmap = MapWire(StringWire, IntWire)
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
          case StateView.Playing(phase:PhaseView, currentPlayer: UserId, diceView:Vector[DiceView], button:Vector[ButtonView]) =>
            Obj("type" -> "Playing",
            "phase" -> PhaseViewFormat.encode(phase),
            "currentPlayer" -> currentPlayer,
            "diceView" -> VectorWire(DiceViewFormat).encode(diceView),
            "buttonView" -> VectorWire(ButtonViewFormat).encode(button)
            )
          case StateView.Finished(winnerId:UserId) =>
            Obj("type" -> "Finished",
            "winnerId" -> winnerId)

      override def decode(js: Value): Try[StateView] = Try:
        js("type").str match
          case "Playing" =>
            StateView.Playing(
              PhaseViewFormat.decode(js("phase")).get,
              js("currentPlayer").str,
              VectorWire(DiceViewFormat).decode(js("dice")).get,
              VectorWire(ButtonViewFormat).decode(js("button")).get
            )
          case "Finished" =>
            StateView.Finished(js("winnerId").str)
//================================ PHASE VIEW WIRE ==============================
    override object PhaseViewFormat extends WireFormat[PhaseView]:
      override def encode(phaseView: PhaseView): Value =
      phaseView match
        case PhaseView.Starting => Obj("type" -> "Starting")
        case PhaseView.SelectingDice => Obj("type" -> "SelectingDice")
        case PhaseView.ViewingDice => Obj("type" -> "ViewingDice")
        case PhaseView.SkullEnd => Obj("type" -> "SkullEnd")
        case PhaseView.SavingEnd => Obj("type" -> "SavingEnd")
        case PhaseView.Waiting => Obj("type" -> "Waiting")
      
      override def decode(js: Value): Try[PhaseView] = Try:
        js("type").str match
          case "Starting" => PhaseView.Starting
          case "SelectingDice" => PhaseView.SelectingDice
          case "ViewingDice" => PhaseView.ViewingDice
          case "SkullEnd" => PhaseView.SkullEnd
          case "SavingEnd" => PhaseView.SavingEnd
          case "Waiting" => PhaseView.Waiting
          case _ => throw DecodingException(f"Invalid phase view $js")
          
//=============================== BUTTON VIEW WIRE ================================
    override object ButtonViewFormat extends WireFormat[ButtonView]:
      override def encode(buttonView: ButtonView): Value =
        buttonView match
          case ButtonView.Clickable(button:Button) => 
            Obj("type" -> "Clickable", "button" -> button)
          case ButtonView.NonClickable(button:Button) =>
            Obj("type" -> " NonClickable", "button" -> button)
        
      override def decode(js: Value): Try[ButtonView] = Try:
        js("type").str match
          case "Clickable" => ButtonView.Clickable(js("button").str)
          case "NonClickable" => ButtonView.NonClickable(js("button").str)

//============================== BUTTON ID WIRE ====================================
    override object ButtonIdFormat extends WireFormat[ButtonType]:
      override def encode(button: ButtonType): Value =
        button match
          case ButtonType.Roll => Obj("type" -> "Roll")
          case ButtonType.End => Obj("type" -> "End")
      
      override def decode(js: Value): Try[ButtonType] = Try:
        js("type").str match
          case "Roll" => ButtonType.Roll
          case "End" => ButtonType.End
          case _ => throw DecodingException(f"Invalid button id $js")
//============================== DICE VIEW WIRE ====================================
    override object DiceViewFormat extends WireFormat[DiceView]:

      override def encode(diceView: DiceView): Value =
        diceView match
          case DiceView.Selected(dice:Dice) =>
            Obj("type" -> "Selected", "dice" -> DiceFormat.encode(dice))
          case DiceView.Unselected(dice:Dice) =>
            Obj("type" -> "Unselected", "dice" -> DiceFormat.encode(dice))
          case DiceView.NonClickable(dice:Dice) =>
            Obj("type" -> "NonClickable", "dice" -> DiceFormat.encode(dice))
      
      override def decode(js: Value): Try[DiceView] = Try:
        js("type").str match
          case "Selected" => DiceView.Selected(DiceFormat.decode(js("dice")).get)
          case "Unselected" => DiceView.Unselected(DiceFormat.decode(js("dice")).get)
          case "NonClickable" => DiceView.NonClickable(DiceFormat.decode(js("dice")).get)     

//============================== DICE WIRE ====================================
    override object DiceFormat extends WireFormat[Dice]:
      override def encode(dice: Dice): Value =
        dice match
          case Dice.Skull => Obj("dice" -> "Skull")
          case Dice.Diamond => Obj("dice" -> "Diamond")
          case Dice.Coin => Obj("dice" -> "Coin")
          case Dice.Sword => Obj("dice" -> "Sword")
          case Dice.Monkey => Obj("dice" -> "Monkey")
          case Dice.Parrot => Obj("dice" -> "Parrot")
      
      override def decode(js: Value): Try[Dice] = Try:
        js("dice").str match
          case "Skull" => Dice.Skull
          case "Diamond" => Dice.Diamond
          case "Coin" => Dice.Coin
          case "Sword" => Dice.Sword
          case "Monkey" => Dice.Monkey
          case "Parrot" => Dice.Parrot
          case _ => throw DecodingException(f"Invalid dice $js")
