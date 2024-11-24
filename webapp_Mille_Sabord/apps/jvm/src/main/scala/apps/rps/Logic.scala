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

  /** Creates a new application state. */
  override def init(clients: Seq[UserId]): State =
    State(
      players = clients.toVector,
      phase = Phase.SelectingDice,
      dices = List.fill(8)(Dice.Empty).toVector,
      selectedDice = Set(),
      score = clients.map(_ -> 0).toMap, 
    )

    //How does the state changes upon action
  override def transition(state: State)(userId: UserId, event: Event): Try[Seq[Action[State]]] = Try : 
    state.phase match 
      case Phase.Done => 
        throw IllegalMoveException("Game is over !")
      case Phase.ViewingHands => 
        throw IllegalMoveException("Don't chose a hand while looking at them !")
      case Phase.SelectingHand => 
        val State(players,phase,versusHands,score,round) = state 
        val Event.HandSelected(hand) = event.asInstanceOf[Event.HandSelected]
        assert(versusHands.size >= 0 && versusHands.size <= players.size)
        //if state.players.head != userId then 
          //throw NotYourTurnException()
        if versusHands.contains(userId) then 
          throw IllegalMoveException("Don't choose two hands")

        val newVersusHands = versusHands + (userId -> hand)
        //a fight can start between the players
        if newVersusHands.size == players.size then 
          //Get the Id of the opponents
          //val  vsUserId = players.filter(_ != userId)
          //val res = hand.scoreAgainst(versusHands(vsUserId))
          //val newScore = score + (userId -> (score(userId) + res))
          val newScore = fight(players, newVersusHands)
          val newRound = round + 1
          val newState = State(players, Phase.ViewingHands, newVersusHands, newScore,newRound)
          if newRound < 3 then 
            val finalState = State(players, Phase.SelectingHand, Map.empty, newScore, newRound)

            val SHOW_HANDS_PAUSE_MS = 2500
            Seq(  
              Action.Render(newState),
              Action.Pause(SHOW_HANDS_PAUSE_MS),
              Action.Render(finalState)  
            )
          else 
            val finalState = State(players.tail :+ players.head, Phase.Done, Map.empty, newScore, newRound)

            val SHOW_HANDS_PAUSE_MS = 2500
            Seq(  
              Action.Render(newState),
              Action.Pause(SHOW_HANDS_PAUSE_MS),
              Action.Render(finalState)  
            )

         
        //We should wait for the other player to choose its hand
        else 
          val newVsHandsState = state.copy(versusHands = newVersusHands)
          Seq(Action.Render(newVsHandsState))
        

  /** How does the game should act with the current action */
  override def project(state: State)(userId: UserId): View =
    val State(players,phase,dices,selectedDice,score) = state 

    val stateView :StateView = 
      phase match 
        /** A player is selecting dice, the other are waiting*/
        case Phase.SelectingDice =>
            val phaseView =  
              if userId == players.head then PhaseView.SelectingDice else PhaseView.Waiting
            /** Is the player starting her turn ? */
            if(dices.contains(Dice.Empty)) then 
              val diceView = dices.map(dice => DiceView.Unselected(dice) ).toVector
              val buttonView = Vector(ButtonView.Clickable(Button.Roll), ButtonView.NonClickable(Button.End))
            else
              //TODO: should make visible selected dices
              val diceView = dices.map(dice => if dice == Dice.Skull then DiceView.Skull(dice) else DiceView.Unselected(dice)).toVector
              val buttonView = Vector(ButtonView.Clickable(Button.Roll), ButtonView.Clickable(Button.End))
            StateView.Playing(phaseView,userId,diceView,buttonView)

        /**Players are viewing results of rerolled dices*/
        case Phase.ViewingDice => 
            /** Players has 3 skulls and loss her turn --> SkullEnd*/
            if dices.foldLeft(0)((prev,next) => next + (if dice == Dice.Skull then 1 else 0)) == 3 then 
              val diceView = dices.map(dice => DiceView.Skull(dice)).toVector
              val buttonView = Vector(ButtonView.NonClickable(Button.Roll), ButtonView.NonClickable(Button.End))
              StateView.Playing(PhaseView.SkullEnd,userId,diceView,buttonView)
            else 
              val diceView = dices.map(dice => if dice == Dice.Skull then DiceView.Skull(dice) else DiceView.Unselected(dice)).toVector
              val buttonView = Vector(ButtonView.NonClickable(Button.Roll), ButtonView.NonClickable(Button.End))
              StateView.Playing(PhaseView.Waiting,userId,diceView,buttonView)

              //TODO: How to trigger the SavingEnd ? Should I store the button selected ? 

        /** The game is done and we show who win*/
        case Phase.Done => 
            val winnerId = score.maxBy(_._2)._2
            StateView.Finished(winnerId)

        
    
    View(stateView,score)



  