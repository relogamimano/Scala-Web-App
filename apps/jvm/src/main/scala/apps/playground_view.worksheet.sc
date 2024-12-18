import scala.util.Random

type UserId = String

/** Dice are simply strings in this version of the game (use Emoji!) */
type Dice = String
type DiceId = Int


/** Button are the string with the indication text of the button */
type Button = String
enum ButtonId: 
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
  def randomDice(): Dice=
    //Get a random number between the interval [1,7[
    val randomDiceIdx = Random.between(1, 7)
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
case class View(
    stateView: StateView,
    scoresView: ScoresView
)

enum StateView:
  /** The game is ongoing. */
  case Playing(phase: PhaseView, currentPlayer: UserId, diceView: Vector[DiceView], buttonView: Vector[ButtonView])

  /** The game is over (only one winner of the game possible) */
  case Finished(winnerId: UserId)

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
  case Skull(dice: Dice)


enum ButtonView: 
  case Clickable(button: Button)

  /** NonClickable button can't be selected and have a lower opacity*/
  case NonClickable(button: Button)

  //case Selected(button: Button) //Can implemented if we have to do animation for clicking on button

enum Event:
  /** A player has selected a dice. */
  case DiceClicked(diceId: DiceId)

  /** A player has clicked on a button. */
  case ButtonClicked(buttonId: ButtonId)


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
  dices : Vector[Dice],
  selectedDice : Set[DiceId],
  score: Map[UserId, Int]
)


//Set up the tests
val dices = List.fill(8)(Dice.Empty.randomDice()).toVector
val reroll_dices = dices.map(dice => dice.randomDice())
val selectedDice = Set(1,2)
val viewDices = dices.zipWithIndex.map((dice, id) => 
  if selectedDice.contains(id) then DiceView.Selected(dice) 
  else if dice != Dice.Skull then DiceView.Unselected(dice)
  else DiceView.Skull(dice))

val buttonView = Vector(ButtonView.Clickable(Button.Roll),ButtonView.Clickable(Button.End))
val currentPlayer: UserId = "2" 


PhaseView.SelectingDice

StateView.Playing(PhaseView.SelectingDice,currentPlayer, viewDices, buttonView)

val UID0: UserId = "0"
val UID1: UserId = "1"
val v1 = View(StateView.Finished(UID0, UID0), Map())
val v2 = View(StateView.Finished(UID1, UID0), Map())    
v1 == v2

val e1 = Event.ButtonClicked(ButtonId.Roll)
val e2 = Event.ButtonClicked(ButtonId.End)
e1 == e2

for diceId <- 0 to 8 do 
  Event.DiceClicked(diceId)

ButtonId.Roll