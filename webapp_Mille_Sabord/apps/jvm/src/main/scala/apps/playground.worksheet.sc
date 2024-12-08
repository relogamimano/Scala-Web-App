import scala.util.Random

val randomDiceIdx = Random.between(1, 7)
2+2
type Dice = String
  //test de commentaires pour git 
object Dice:
  val Skull = "💀"
  val Diamond = "💎"
  val Coin = "📀"
  val Sword = "🔪"
  val Monkey = "🐵" // "🐒"
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

enum DiceView:
  /** Selected dice have a green square around them*/
  case Selected(dice: Dice)

  /** Unselected dice just have default view*/
  case Unselected(dice: Dice)

  /** Skull dice have a gray square around them and a lower opacity*/
  case NonClickable(dice: Dice)

  /** Get the dice from the DiceView */
  def getDice: Dice = this match
    case Selected(dice) => dice
    case Unselected(dice) => dice
    case NonClickable(dice) => dice


List.fill(8)(Dice.Empty).toVector

val score = Map("1"-> 2, "2"-> 4,"3" ->8)
score.maxBy(_._2)._1

Dice.Empty.randomDice()

val dices = List.fill(8)(Dice.Empty.randomDice()).toVector

val diceView = dices.map(dice => if dice == Dice.Skull then DiceView.NonClickable(dice) else DiceView.Unselected(dice)).toVector
diceView

val selectedDice = Set(3,5)

val view = 
  dices.zipWithIndex.map((dice, id) => 
    if dice == Dice.Skull then DiceView.NonClickable(dice)
    else if  selectedDice.contains(id) then DiceView.Selected(dice) 
    else DiceView.Unselected(dice))
view
val select = Set()
select.isEmpty

dices.map(dice => dice.randomDice())


val seed = 42
val random = new Random(seed)


val rdmInt = Random.nextInt()
val rdm = new Random(rdmInt)

def testRdm(seed: Int = Random.nextInt()): Int = 
  val rdm = new Random(seed)
  rdm.nextInt()

testRdm()

val emptyDice = DiceView.NonClickable(Dice.Empty)
emptyDice match
  case DiceView.NonClickable(dice) => dice
  case _ => "No dice"

view(5)
//Get the dice value inside of view(5)

val selectedView = view.updated(5, DiceView.Selected(view(5).getDice))


(view.zipWithIndex.forall((diceV, idx) => 
  if diceV.getDice == Dice.Skull then (diceV.isInstanceOf[DiceView.NonClickable])
  else if selectedDice.contains(idx) then (diceV.isInstanceOf[DiceView.Selected])
  else (diceV.isInstanceOf[DiceView.Unselected])))

enum ButtonType: 
  case Roll
  case End

val buttonTest = ButtonType.End

(0 to 7).toSet

def testSet(set: Set[Int]) = 
  for ele <- set do println(ele)

testSet((0 to 7).toSet)
Set(4)

