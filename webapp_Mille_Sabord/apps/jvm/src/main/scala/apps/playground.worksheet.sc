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

enum DiceView:
  /** Selected dice have a green square around them*/
  case Selected(dice: Dice)

  /** Unselected dice just have default view*/
  case Unselected(dice: Dice)

  /** Skull dice have a gray square around them and a lower opacity*/
  case Skull(dice: Dice)


List.fill(8)(Dice.Empty).toVector

val score = Map("1"-> 2, "2"-> 4,"3" ->8)
score.maxBy(_._2)._1

Dice.Empty.randomDice()

val dices = List.fill(8)(Dice.Empty.randomDice()).toVector

val diceView = dices.map(dice => if dice == Dice.Skull then DiceView.Skull(dice) else DiceView.Unselected(dice)).toVector
diceView

val selectedDice = Set(3,5)

dices.zipWithIndex.map((dice, id) => 
  if selectedDice.contains(id) then DiceView.Selected(dice) 
  else if dice != Dice.Skull then DiceView.Unselected(dice)
  else DiceView.Skull)

val select = Set()
select.isEmpty

dices.map(dice => dice.randomDice())
