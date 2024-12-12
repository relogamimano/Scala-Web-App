package apps
package rps

import cs214.webapp.*
import cs214.webapp.server.{StateMachine}
import ujson.Value

import scala.annotation.unused
import scala.util.{Random, Try}
import scala.collection.MapOps
import scala.util.Random

class Logic extends StateMachine[Event, State, View]:

  val appInfo: AppInfo = AppInfo(
    id = "rps",
    name = "Mille Sabords",
    description = "Mille Sabords is a pirate theme dice game " +
      "where you gain points by getting the same symbols on your dice.",
    year = 2024
  )

  override val wire = rps.Wire
  // Feel free to tweak this value!
  private val VIEW_DICE_PAUSE_MS = 2500
  private val SHOW_TURN_END_PAUSE_MS = 2500

  /** Creates a new application state. */
  override def init(clients: Seq[UserId]): State =
    initSeed(clients)
  
  def initSeed(clients: Seq[UserId], initSeed: Option[Int] = None): State =
    State(
      players = clients.toVector,
      phase = Phase.StartingTurn,
      dices = List.fill(8)(Dice.Empty).toVector,
      selectedDices = Set(),
      score = clients.map(_ -> 0).toMap, 
      seed = new Random(initSeed.getOrElse(Random.nextInt())) //If there are any specified seed, we use a random one (mostly useful for testing)
    )

  def calculateScore(dices: Vector[Dice]): Int = {
  // Check for occurences
  val counts = dices.groupBy(identity).view.mapValues(_.size)

  // Points for three similar emojis
  val bonusPoints = counts.collect {
    case (Dice.Skull, count) => 0
    case (_, count) if count >= 3 => 100
    case (_, count) if count >= 4 => 200
    case (_, count) if count >= 5 => 500
    case (_, count) if count >= 6 => 1000
    case (_, count) if count >= 7 => 2000
    case (_, count) if count >= 8 => 4000
  }.sum

  // Points for Diamond And Coins
  val normalPoints = dices.foldLeft(0) {
    case (acc, Dice.Coin) => acc + 100
    case (acc, Dice.Diamond) => acc + 100
    case (acc, _) => acc + 0
  }

  // Total points
   normalPoints + bonusPoints
}


  def rollDices(dices: Vector[Dice], selectedDice: Set[DiceId],rdmSeed: Random): Vector[Dice] = 
    dices.zipWithIndex.map((dice, id) => 
      if dice == Dice.Skull && selectedDice.contains(id) then throw IllegalMoveException("Can't     reroll a skull")
  
  def isTurnLost(dices: Vector[Dice]): Boolean = 
    // rename for turn over 
    dices.foldLeft(0)((prev,next) => prev + (if next == Dice.Skull then 1 else 0)) == 3
  
  def isGameWon(score:Int): Boolean = 
    score >= 3000


    //How does the state changes upon action
  override def transition(state: State)(userId: UserId, event: Event): Try[Seq[Action[State]]] = Try : 
    val State(players,phase,dices,selectedDice,score,seed) = state 
    state.phase match 
      case Phase.EndingGame => 
        throw IllegalMoveException("Game is over !")
      case Phase.EndingTurn => 
        throw IllegalMoveException("Wait your turn !")
      case Phase.ViewingDice => 
        throw IllegalMoveException("Don't click now!")
      case Phase.StartingTurn =>
        event match
          case Event.ButtonClicked(buttonId) => 
            buttonId match 
              case ButtonType.End => 
                throw IllegalMoveException("Roll the dice first!")
              case ButtonType.Roll =>  
                val rolledDices = dices.map(_.randomDice(seed))
                if isTurnLost(rolledDices) then 
                  val initDice = dices.map(_ => Dice.Empty)
                  val endState = state.copy(phase = Phase.EndingTurn,selectedDices = Set())
                  val newPlayerState = State(players.tail :+ players.head,Phase.StartingTurn,initDice,Set(),score,seed)
                  Seq(
                    Action.Render(state),
                    Action.Pause(VIEW_DICE_PAUSE_MS),
                    Action.Render(endState),
                    Action.Pause(SHOW_TURN_END_PAUSE_MS),
                    Action.Render(newPlayerState)
                    )
                  else
                    val newState = state.copy(phase = Phase.SelectingDice,dices = rolledDices)
                    Seq(Action.Render(newState))
                val newState = state.copy(phase = Phase.SelectingDice,dices = rolledDices)
                Seq(Action.Render(newState))
          case Event.DiceClicked(diceId) => 
            throw IllegalMoveException("Roll the dice first!")
      case Phase.SelectingDice => 
        event match 
          case Event.ButtonClicked(buttonId) => 
            buttonId match
              case ButtonType.End =>   
                val initDice = dices.map(_ => Dice.Empty)
                val turnScore = calculateScore(dices)
                val newScore = score + (userId -> (score(userId) + turnScore))
                val userScore: Int = newScore(userId)
                if isGameWon(userScore) then 
                  val endPhase = state.copy(phase = Phase.EndingGame)
                    Seq(Action.Render(endPhase))
                else 
                  val endState = State(players,Phase.EndingTurn,dices,Set(),newScore,seed)
                  val newPlayerState = State(players.tail :+ players.head,Phase.StartingTurn,initDice,Set(),newScore,seed)
                  Seq(
                      Action.Render(endState),
                      Action.Pause(SHOW_TURN_END_PAUSE_MS),
                      Action.Render(newPlayerState)
                      )
              case ButtonType.Roll => 
                if selectedDice.isEmpty then throw IllegalMoveException("Select Dice First!")
                else
                  val rolledDice = rollDices(dices,selectedDice,seed)
                  val viewState = state.copy(phase = Phase.ViewingDice,dices = rolledDice)

                  if isTurnLost(rolledDice) then 
                    val initDice = dices.map(_ => Dice.Empty)
                    val endState = viewState.copy(phase = Phase.EndingTurn,selectedDices = Set())
                    val newPlayerState = State(players.tail :+ players.head,Phase.StartingTurn,initDice,Set(),score,seed)
                    Seq(
                      Action.Render(viewState),
                      Action.Pause(VIEW_DICE_PAUSE_MS),
                      Action.Render(endState),
                      Action.Pause(SHOW_TURN_END_PAUSE_MS),
                      Action.Render(newPlayerState)
                    )
                  else
                    val newTurnState = State(players,Phase.SelectingDice,rolledDice,Set(),score,seed)
                    Seq(
                      Action.Render(viewState),
                      Action.Pause(VIEW_DICE_PAUSE_MS),
                      Action.Render(newTurnState)
                    )

          case Event.DiceClicked(diceId) => 
                      val newSelectedDice = 
                        if selectedDice.contains(diceId) then 
                          selectedDice.filterNot(_ == diceId)
                        else 
                          selectedDice + diceId
                      val newState = state.copy(selectedDices = newSelectedDice)
                      Seq(Action.Render(newState))  

  /** How does the game should act with the current action */
  override def project(state: State)(userId: UserId): View =
    val State(players,phase,dices,selectedDice,score,seed) = state 

    val stateView :StateView = 
      /** The game is done and we show who win*/
      if phase == Phase.EndingGame then 
        val winnerId: UserId = score.maxBy(_._2)._1
        StateView.Finished(winnerId)
      else 
        /** It's not the player turn and she should wait*/
        if userId != players.head then
          val phaseView = PhaseView.Waiting
          //All dices are viewed as "NonClickable", meaning that they have a grey surrounding square and lower opacity to indicates that they can't be selected
          val diceView = dices.map(dice => DiceView.NonClickable(dice) ).toVector
          val buttonView = Vector(ButtonView.NonClickable(Button.Roll), ButtonView.NonClickable(Button.End))
          StateView.Playing(phaseView,userId,diceView,buttonView)
        else
        phase match 
          /** The active player is starting her turn, the other are waiting*/
          case Phase.StartingTurn => 
            //All dices are viewed as "NonClickable", meaning that they have a grey surrounding square and lower opacity to indicates that they can't be selected
            val diceView = dices.map(dice => DiceView.NonClickable(dice) ).toVector
            val buttonView = Vector(ButtonView.Clickable(Button.Roll), ButtonView.NonClickable(Button.End))
            StateView.Playing(PhaseView.Starting,userId,diceView,buttonView)

          /** A player is selecting dice, the other are waiting*/
          case Phase.SelectingDice =>
              val diceView = dices.zipWithIndex.map((dice, id) => 
                if dice == Dice.Skull then DiceView.NonClickable(dice)
                else if  selectedDice.contains(id) then DiceView.Selected(dice) 
                else DiceView.Unselected(dice))
              val buttonView = 
                //The player can reroll the dices only if she has at least selected one dice
                if selectedDice.isEmpty then 
                  Vector(ButtonView.NonClickable(Button.Roll), ButtonView.Clickable(Button.End))
                else 
                  Vector(ButtonView.Clickable(Button.Roll), ButtonView.Clickable(Button.End))
                  
              StateView.Playing(PhaseView.SelectingDice,userId,diceView,buttonView)

          /**Players are viewing results of rerolled dices*/
          case Phase.ViewingDice => 
            val diceView = dices.map(dice => if dice == Dice.Skull then DiceView.NonClickable(dice) else DiceView.Unselected(dice)).toVector
            val buttonView = Vector(ButtonView.NonClickable(Button.Roll), ButtonView.NonClickable(Button.End))
            StateView.Playing(PhaseView.ViewingDice,userId,diceView,buttonView)

          /** The player's turn is over and we have to count the points marked if any*/
          case Phase.EndingTurn => 
            /** Player has 3 skulls and loss her turn --> SkullEnd*/
            if isTurnLost(dices) then 
              val diceView = dices.map(dice => DiceView.NonClickable(dice)).toVector
              val buttonView = Vector(ButtonView.NonClickable(Button.Roll), ButtonView.NonClickable(Button.End))
              StateView.Playing(PhaseView.SkullEnd,userId,diceView,buttonView)
            /** Player chose to end her turn and save her score --> SavingEnd*/
            else  
              //All dices are viewed as "NonClickable", meaning that they have a grey surrounding square and lower opacity to indicates that they can't be selected
              val diceView = dices.map(dice => DiceView.NonClickable(dice)).toVector
              val buttonView = Vector(ButtonView.NonClickable(Button.Roll), ButtonView.NonClickable(Button.End))
              StateView.Playing(PhaseView.SavingEnd,userId,diceView,buttonView)

          /** This condition should have been verified and reached before if true*/
          case Phase.EndingGame => 
            throw AssertionError("Unreachable")
              
    
    View(stateView,score)



  