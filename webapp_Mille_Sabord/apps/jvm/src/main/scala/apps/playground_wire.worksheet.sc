import ujson.*
import scala.util.{Failure, Success, Try}
import io.undertow.util.FlexBase64.Decoder
import scala.util.Random


//* Scala Logic import */
type UserId = String

/** Dice are simply strings in this version of the game (use Emoji!) */
type Dice = String
type DiceId = Int


/** Button are the string with the indication text of the button */
type Button = String
enum ButtonType: 
  case Roll
  case End

object Dice:
  val Skull = "💀"
  val Diamond = "💎"
  val Coin = "📀"
  val Sword = "🔪"
  val Monkey = "🐵" //"🐒"
  val Parrot = "🐦"
  val Empty = "❓"

extension (dice: Dice)
  def randomDice(randomSeed: Random): Dice=
    //Get a random number between the interval [1,7[
    val randomDiceIdx = randomSeed.between(1, 7)
    randomDiceIdx match 
      case 1 => Dice.Skull
      case 2 => Dice.Diamond
      case 3 => Dice.Coin
      case 4 => Dice.Sword
      case 5 => Dice.Monkey
      case 6 => Dice.Parrot
      
object Button:
  val Roll = "Roll the dice"
  val End = "End my turn"


/** A view of Mille Sabord's state for a specific client.
  *
  * The UI alternates between two views: 
  * - the "playing" phase where the player select dice, reroll them and can end its turn
  * - the "scoring" phase where the player turn ends and its points for the turn are computed and displayed
  *
  * @param stateView
  *   A projection of the current phase of the game.
  * @param scoresView
  *   The score of each player.
  */
//??
case class View(
    stateView: StateView,
    scoresView: ScoresView
)

enum StateView:
  /** The game is ongoing. */
  case Playing(phase: PhaseView, currentPlayer: UserId, diceView: Vector[DiceView], buttonView: Vector[ButtonView])

  /** The game is over (only one winner of the game possible) */
  case Finished(winnerId: UserId) //This isn't a Set of UserId as, in this game, it can only be one winner

//??
enum PhaseView:
  /** It's the start of your turn, roll the dice for the first time. */
  case Starting

  /** It's your turn to select dice to rethrown. */
  case SelectingDice

  /** It's your turn and you can view the result of your reroll. */
  case ViewingDice

  /** Your turn came to an end because you get 3 skulls */
  case SkullEnd

  /** Your turn came to an end because you chose to stop it and save your score*/
  case SavingEnd

  /** It's another player's turn: we're waiting for them rethrow dice or stop. */
  case Waiting

type ScoresView = Map[UserId, Int]


enum DiceView:
  /** Selected dice have a green square around them*/
  case Selected(dice: Dice)

  /** Unselected dice just have default view*/
  case Unselected(dice: Dice)

  /** Skull dice have a gray square around them and a lower opacity*/
  /** This is also used to indicate that dices can't be selected */ 
  case NonClickable(dice: Dice)

  /** Get the dice from the DiceView */
  def getDice: Dice = this match
    case Selected(dice) => dice
    case Unselected(dice) => dice
    case NonClickable(dice) => dice


enum ButtonView: 
  case Clickable(button: Button)

  /** NonClickable button can't be selected and have a lower opacity*/
  case NonClickable(button: Button)

  //case Selected(button: Button) //Can implemented if we have to do animation for clicking on button

enum Event:
  /** A player has selected a dice. */
  case DiceClicked(diceId: DiceId)

  /** A player has clicked on a button. */
  case ButtonClicked(buttonId: ButtonType)


enum Phase:
  case StartingTurn
  case SelectingDice
  /** View when the player is rolling dice. */
  case ViewingDice 
  /** View when the player is ending her turn -> triggered by SkullEnd or SavingEnd */
  case EndingTurn 
  case EndingGame

case class State (  
  players: Vector[UserId],
  phase: Phase,
  dices : Vector[Dice], //The vector should always contains 8 dices
  selectedDices : Set[DiceId],
  score: Map[UserId, Int],
  seed: Random
)

//*Wire import */

def encode(event: Event): Value =
  event match 
    case Event.DiceClicked(diceId) => Obj("type" -> "DiceClicked", "diceId" -> diceId)
    case Event.ButtonClicked(buttonId) => Obj("type" -> "ButtonClicked", "buttonId" -> encodeButton(buttonId))        

def decode(json: Value): Try[Event] = Try:
  json("type").str match
    case "DiceClicked" => Event.DiceClicked(json("diceId").num.toInt)
    case "ButtonClicked" => Event.ButtonClicked(decodeButton(json("buttonId")).get)
    case _ => throw Exception(f"Invalid memory event $json")

def encodeButton(button: ButtonType): Value =
  button match
    case ButtonType.Roll => Obj("type" -> "Roll")
    case ButtonType.End => Obj("type" -> "End")

def decodeButton(js: Value): Try[ButtonType] = Try:
  js("type").str match
    case "Roll" => ButtonType.Roll
    case "End" => ButtonType.End
    case _ => throw Exception(f"Invalid button id $js")


val eventDice = Event.DiceClicked(3)
val eventButton = Event.ButtonClicked(ButtonType.End)

decode(encode(eventDice))

decode(encode(eventButton))


// Encode and decode the View
val scoresmap = MapWire(StringWire, IntWire)
def encodeView(view: View): Value =
  Obj(
    "stateView" -> encodeStateView(view.stateView),
    "scoresView" -> scoresmap.encode(view.scoresView)
  )
def decodeView(js: Value): Try[View] = Try:
  View(
    decodeView(js("stateView")).get,
    scoresmap.decode(js("scoresView")).get
  )
//============================= STATE VIEW WIRE =================================
def encodeStateView(stateView: StateView): Value = 
  stateView match
    case StateView.Playing(phase:PhaseView, currentPlayer: UserId, diceView:Vector[DiceView], button:Vector[ButtonView]) =>
      Obj("type" -> "Playing",
      "phase" -> encodePhaseView(phase),
      "currentPlayer" -> currentPlayer,
      "diceView" -> encodeDiceView(diceView),
      "buttonView" -> encodeButtonView(button)
      )
    case StateView.Finished(winnerId:UserId) =>
      Obj("type" -> "Finished",
      "winnerId" -> winnerId)

def decodeStateView(js: Value): Try[StateView] = Try:
  js("type").str match
    case "Playing" =>
      StateView.Playing(
        decodeView(js("phase")).get,
        js("currentPlayer").str,
        decodeDiceView(js("dice")).get,
        decodeButtonView(js("button")).get
      )
    case "Finished" =>
      StateView.Finished(js("winnerId").str)

//================================ PHASE VIEW WIRE ==============================
def encodePhaseView(phaseView: PhaseView): Value =
  phaseView match
    case PhaseView.Starting => Obj("type" -> "Starting")
    case PhaseView.SelectingDice => Obj("type" -> "SelectingDice")
    case PhaseView.ViewingDice => Obj("type" -> "ViewingDice")
    case PhaseView.SkullEnd => Obj("type" -> "SkullEnd")
    case PhaseView.SavingEnd => Obj("type" -> "SavingEnd")
    case PhaseView.Waiting => Obj("type" -> "Waiting")
    
def decodePhaseView(js: Value): Try[PhaseView] = Try:
  js("type").str match
    case "Starting" => PhaseView.Starting
    case "SelectingDice" => PhaseView.SelectingDice
    case "ViewingDice" => PhaseView.ViewingDice
    case "SkullEnd" => PhaseView.SkullEnd
    case "SavingEnd" => PhaseView.SavingEnd
    case "Waiting" => PhaseView.Waiting
    case _ => throw Exception(f"Invalid phase view $js")
        
//=============================== BUTTON VIEW WIRE ================================
def encodeButtonView(buttonView: ButtonView): Value =
  buttonView match
    case ButtonView.Clickable(button:Button) => 
      Obj("type" -> "Clickable", "button" -> button)
    case ButtonView.NonClickable(button:Button) =>
      Obj("type" -> " NonClickable", "button" -> button)
      
def decodeButtonView(js: Value): Try[ButtonView] = Try:
  js("type").str match
    case "Clickable" => ButtonView.Clickable(js("button").str)
    case "NonClickable" => ButtonView.NonClickable(js("button").str)

//============================== BUTTON ID WIRE ====================================
def encodeButtonType(button: ButtonType): Value =
  button match
    case ButtonType.Roll => Obj("type" -> "Roll")
    case ButtonType.End => Obj("type" -> "End")

def decodeButtonType(js: Value): Try[ButtonType] = Try:
  js("type").str match
    case "Roll" => ButtonType.Roll
    case "End" => ButtonType.End
    case _ => throw Exception(f"Invalid button id $js")
//============================== DICE VIEW WIRE ====================================
def encodeDiceView(diceView: DiceView): Value =
  diceView match
    case DiceView.Selected(dice:Dice) =>
      Obj("type" -> "Selected", "dice" -> encodeDice(dice))
    case DiceView.Unselected(dice:Dice) =>
      Obj("type" -> "Unselected", "dice" -> encodeDice(dice))
    case DiceView.NonClickable(dice:Dice) =>
      Obj("type" -> "NonClickable", "dice" -> encodeDice(dice))

def decodeDiceView(js: Value): Try[DiceView] = Try:
  js("type").str match
    case "Selected" => DiceView.Selected(decodeDice(js("dice")).get)
    case "Unselected" => DiceView.Unselected(decodeDice(js("dice")).get)
    case "NonClickable" => DiceView.NonClickable(decodeDice(js("dice")).get)     

//============================== DICE WIRE ====================================
def encodeDice(dice: Dice): Value =
  dice match
    case Dice.Skull => Obj("dice" -> "Skull")
    case Dice.Diamond => Obj("dice" -> "Diamond")
    case Dice.Coin => Obj("dice" -> "Coin")
    case Dice.Sword => Obj("dice" -> "Sword")
    case Dice.Monkey => Obj("dice" -> "Monkey")
    case Dice.Parrot => Obj("dice" -> "Parrot")
    case Dice.Empty => Obj("dice" -> "Empty")
    
def decodeDice(js: Value): Try[Dice] = Try:
  js("dice").str match
    case "Skull" => Dice.Skull
    case "Diamond" => Dice.Diamond
    case "Coin" => Dice.Coin
    case "Sword" => Dice.Sword
    case "Monkey" => Dice.Monkey
    case "Parrot" => Dice.Parrot
    case "Empty" => Dice.Empty
    case _ => throw Exception(f"Invalid dice $js")