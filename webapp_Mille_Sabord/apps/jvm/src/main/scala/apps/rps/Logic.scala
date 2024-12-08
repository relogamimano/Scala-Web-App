package apps
package rps

import cs214.webapp.*
import cs214.webapp.server.{StateMachine}
import ujson.Value

import scala.annotation.unused
import scala.util.{Random, Try}
import scala.collection.MapOps

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
    State(
      players = clients.toVector,
      phase = Phase.StartingTurn,
      dices = List.fill(8)(Dice.Empty).toVector,
      selectedDice = Set(),
      score = clients.map(_ -> 0).toMap, 
    )

  def calculateScore(dices: Vector[Dice]): Int = 
    dices.foldLeft(0){
      case (acc, Dice.Skull) => acc + 0 // Pas de points pour les crânes
      case (acc, _)          => acc + 100 // +100 pour tous les autres symboles
    }

  def rollDices(dices: Vector[Dice], selectedDice: Set[DiceId]): Vector[Dice] = 
    dices.zipWithIndex.map((dice, id) => 
      if dice == Dice.Skull then throw IllegalMoveException("Can't reroll a skull")
      else if selectedDice.contains(id) then dice.randomDice() 
      else dice)
  
  def isGameLost(dices: Vector[Dice]): Boolean = 
    dices.foldLeft(0)((prev,next) => prev + (if next == Dice.Skull then 1 else 0)) == 3

    //How does the state changes upon action
  override def transition(state: State)(userId: UserId, event: Event): Try[Seq[Action[State]]] = Try : 
    val State(players,phase,dices,selectedDice,score) = state 
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
              case ButtonId.End => 
                throw IllegalMoveException("Roll the dice first!")
              case ButtonId.Roll =>  
                val rolledDices = dices.map(_.randomDice())
                val newState = state.copy(phase = Phase.SelectingDice,dices = rolledDices)
                Seq(Action.Render(newState))
          case Event.DiceClicked(diceId) => 
            throw IllegalMoveException("Roll the dice first!")
      case Phase.SelectingDice => 
        event match 
          case Event.ButtonClicked(buttonId) => 
            buttonId match
              case ButtonId.End =>   
                val initDice = dices.map(_ => Dice.Empty)
                val turnScore = calculateScore(dices)
                val newScore = score + (userId -> (score(userId) + turnScore))
                val endState = State(players,Phase.EndingTurn,dices,Set(),newScore)
                val newPlayerState = State(players.tail :+ players.head,Phase.StartingTurn,initDice,Set(),newScore)
                Seq(
                      Action.Render(endState),
                      Action.Pause(SHOW_TURN_END_PAUSE_MS),
                      Action.Render(newPlayerState)
                    )
              case ButtonId.Roll => 
                if selectedDice.isEmpty then throw IllegalMoveException("Select Dice First!")
                else
                  val rolledDice = rollDices(dices,selectedDice)
                  val viewState = state.copy(phase = Phase.ViewingDice,dices = rolledDice)

                  if isGameLost(rolledDice) then 
                    val initDice = dices.map(_ => Dice.Empty)
                    val endState = viewState.copy(phase = Phase.EndingTurn,selectedDice = Set())
                    val newPlayerState = State(players.tail :+ players.head,Phase.StartingTurn,initDice,Set(),score)
                    Seq(
                      Action.Render(viewState),
                      Action.Pause(VIEW_DICE_PAUSE_MS),
                      Action.Render(endState),
                      Action.Pause(SHOW_TURN_END_PAUSE_MS),
                      Action.Render(newPlayerState)
                    )
                  else
                    val newTurnState = State(players,Phase.SelectingDice,rolledDice,Set(),score)
                    Seq(
                      Action.Render(viewState),
                      Action.Pause(VIEW_DICE_PAUSE_MS),
                      Action.Render(newTurnState)
                    )

          case Event.DiceClicked(diceId) => 
            if selectedDice.contains(diceId) then throw IllegalMoveException("Selected a new dice!")
            else 
              val newSelectedDice = selectedDice + diceId
              val newState = state.copy(selectedDice = newSelectedDice)
              Seq(Action.Render(newState))  

        //TO DO 

          // case DiceClicked(diceId)
          //   var selectedDice = State.selectedDice
          //   var selectedDice = selectedDice +:DiceId
          //   // add the option to remove
      //   val State(players,phase,dices,score) = state 
      // case Phase.SelectingDice=> 
      //   val State(players,phase,dices,score) = state 
      //   event match 
      //     // case DiceClicked
      //     // case ButtonClicked
      //   // val Event.HandSelected(hand) = event.asInstanceOf[Event.HandSelected]
      //   //raise exeption if no dice are selected
      //   // passage a viewing dice quand tu touches le bouton 
      //   val Event.HandSelected(hand) = event.asInstanceOf[Event.HandSelected]
      //   assert(versusHands.size >= 0 && versusHands.size <= players.size)
      //   //if state.players.head != userId then 
      //     //throw NotYourTurnException()
      //   if versusHands.contains(userId) then 
      //     throw IllegalMoveException("Don't choose two hands")

      //   val newVersusHands = versusHands + (userId -> hand)
      //   //a fight can start between the players
      //   if newVersusHands.size == players.size then 
      //     //Get the Id of the opponents
      //     //val  vsUserId = players.filter(_ != userId)
      //     //val res = hand.scoreAgainst(versusHands(vsUserId))
      //     //val newScore = score + (userId -> (score(userId) + res))
      //     val newScore = fight(players, newVersusHands)
      //     val newRound = round + 1
      //     val newState = State(players, Phase.ViewingHands, newVersusHands, newScore,newRound)
      //     if newRound < 3 then 
      //       val finalState = State(players, Phase.SelectingHand, Map.empty, newScore, newRound)

      //       val SHOW_HANDS_PAUSE_MS = 2500
      //       Seq(  
      //         Action.Render(newState),
      //         Action.Pause(SHOW_HANDS_PAUSE_MS),
      //         Action.Render(finalState)  
      //       )
      //     else 
      //       val finalState = State(players.tail :+ players.head, Phase.Done, Map.empty, newScore, newRound)

      //       val SHOW_HANDS_PAUSE_MS = 2500
      //       Seq(  
      //         Action.Render(newState),
      //         Action.Pause(SHOW_HANDS_PAUSE_MS),
      //         Action.Render(finalState)  
      //       )

      //   //We should wait for the other player to choose its hand
      //   else 
      //     val newVsHandsState = state.copy(versusHands = newVersusHands)
      //     Seq(Action.Render(newVsHandsState))
        



  /** How does the game should act with the current action */
  override def project(state: State)(userId: UserId): View =
    val State(players,phase,dices,selectedDice,score) = state 

    val stateView :StateView = 
      /** The game is done and we show who win*/
      if phase == Phase.EndingGame then 
        val winnerId: UserId = score.maxBy(_._2)._1
        StateView.Finished(winnerId)
      else 
        /** It's not the player turn and she should wait*/
        if userId != players.head then
          val phaseView = PhaseView.Waiting
          //All dices are viewed as "Skull", meaning that they have a grey surrounding square and lower opacity to indicates that they can't be selected
          val diceView = dices.map(dice => DiceView.Skull(dice) ).toVector
          val buttonView = Vector(ButtonView.NonClickable(Button.Roll), ButtonView.NonClickable(Button.End))
          StateView.Playing(phaseView,userId,diceView,buttonView)
        else
        phase match 
          /** The active player is starting her turn, the other are waiting*/
          case Phase.StartingTurn => 
            //All dices are viewed as "Skull", meaning that they have a grey surrounding square and lower opacity to indicates that they can't be selected
            val diceView = dices.map(dice => DiceView.Skull(dice) ).toVector
            val buttonView = Vector(ButtonView.Clickable(Button.Roll), ButtonView.NonClickable(Button.End))
            StateView.Playing(PhaseView.Starting,userId,diceView,buttonView)

          /** A player is selecting dice, the other are waiting*/
          case Phase.SelectingDice =>
              val diceView = dices.zipWithIndex.map((dice, id) => 
                if dice == Dice.Skull then DiceView.Skull(dice)
                else if selectedDice.contains(id) then DiceView.Selected(dice) 
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
            val diceView = dices.map(dice => if dice == Dice.Skull then DiceView.Skull(dice) else DiceView.Unselected(dice)).toVector
            val buttonView = Vector(ButtonView.NonClickable(Button.Roll), ButtonView.NonClickable(Button.End))
            StateView.Playing(PhaseView.ViewingDice,userId,diceView,buttonView)

          /** The player's turn is over and we have to count the points marked if any*/
          case Phase.EndingTurn => 
            /** Player has 3 skulls and loss her turn --> SkullEnd*/
            if isGameLost(dices) then 
              val diceView = dices.map(dice => DiceView.Skull(dice)).toVector
              val buttonView = Vector(ButtonView.NonClickable(Button.Roll), ButtonView.NonClickable(Button.End))
              StateView.Playing(PhaseView.SkullEnd,userId,diceView,buttonView)
            /** Player chose to end her turn and save her score --> SavingEnd*/
            else  
              //All dices are viewed as "Skull", meaning that they have a grey surrounding square and lower opacity to indicates that they can't be selected
              val diceView = dices.map(dice => DiceView.Skull(dice)).toVector
              val buttonView = Vector(ButtonView.NonClickable(Button.Roll), ButtonView.NonClickable(Button.End))
              StateView.Playing(PhaseView.SavingEnd,userId,diceView,buttonView)

          /** This condition should have been verified and reached before if true*/
          case Phase.EndingGame => 
            throw AssertionError("Unreachable")
              
    
    View(stateView,score)



  