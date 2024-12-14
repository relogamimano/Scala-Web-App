package apps.app142

import cs214.webapp.UserId
// import Dice.*
// import Button.*
import scala.util.Random

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
  val All = Set(Skull, Diamond, Coin, Sword, Monkey, Parrot)

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
  case Finished(winnerId: UserId, currentPlayer: UserId) //This isn't a Set of UserId as, in this game, it can only be one winner

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