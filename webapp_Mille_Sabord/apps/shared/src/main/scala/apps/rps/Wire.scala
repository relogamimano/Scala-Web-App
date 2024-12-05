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
           


// View => StateView => PhaseView
// View => StateView => DiceView => Dice
// View => StateView => ButtonView
// View => ScoresView

//                                     View
//             StateView                              ScoresView
//
// PhaseView    DiceView    ButtonView          
//
//                Dice
    override object PhaseView extends WireFormat[PhaseView]:
      override def encode(phaseView: PhaseView): Value =
        Obj("type" -> phaseView.value())

      override def decode(js: Value): Try[PhaseView] = Try:
        PhaseView.valueOf

    override object DiceViewFormat extends WireFormat[DiceView]:
      override def encode(diceView: DiceView): Value =
        Obj("type" -> diceView.value(), "dice" -> DiceFormat.encode(dice))
      
      override def decode(js: Value): Try[DiceView] = Try:
        DiceView.valueOf(js("type"))(DiceFormat.decode(js("dice")))

    override object DiceFormat extends WireFormat[Dice]:
      override def encode(dice: Dice): Value =
        Obj("type" -> dice.toString())

      override def decode(js: Value): Try[Dice] = Try:
        Dice.valueOf(js("type"))


    
        