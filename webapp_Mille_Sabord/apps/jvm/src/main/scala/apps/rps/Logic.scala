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
    name = "Rock-Paper-Scissors",
    description = "Rock-Paper-Scissors is a hand game where Rock " +
      "crushes Scissors, Scissors cuts Paper, and Paper covers Rock.",
    year = 2024
  )

  override val wire = rps.Wire
  // Feel free to tweak this value!
  private val VIEW_HANDS_PAUSE_MS = 2500

  /** Creates a new application state. */
  override def init(clients: Seq[UserId]): State =
    State(
      players = clients.toVector,
      phase = Phase.SelectingHand,
      versusHands = Map.empty,
      score = clients.map(_ -> 0).toMap, 
      round = 0
    )

  def fight(players: Vector[UserId], versusHands: Map[UserId, Hand]): Map[UserId,Int] =    
    val final_score = 
        for 
            player <- players
            opponents = players.filter(_ != player)
            score = 
                for 
                    opponent <- opponents
                    res = versusHands(player).scoreAgainst(versusHands(opponent))
                yield(res)
        yield (player -> score.toList.foldLeft(0)(_+_))
    final_score.map((str,int) => (str -> int)).toMap


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
        



  // How does the game should act with the current action
  override def project(state: State)(userId: UserId): View =
    val State(players,phase,versusHands,score, round) = state 

    val stateView :PhaseView = 
      phase match 
        case Phase.SelectingHand => 
          //The player has already played and is viewing his hand
          if versusHands.contains(userId) then 
            //If the player has already played he is seeing the selection view but without the ability to chose one hand
            val selectedHand = players.map(id => (id -> versusHands.contains(id))).toMap
            PhaseView.SelectingHand(selectedHand)
          //The player hasn't chosen a hand and should select his hand
          else 
            val selectedHand = players.map(id => (id -> versusHands.contains(id))).toMap
            PhaseView.SelectingHand(selectedHand)
        case Phase.ViewingHands => 
            PhaseView.ViewingHands(versusHands)
        case Phase.Done => 
            PhaseView.ViewingHands(versusHands)
    View(stateView,score)



  