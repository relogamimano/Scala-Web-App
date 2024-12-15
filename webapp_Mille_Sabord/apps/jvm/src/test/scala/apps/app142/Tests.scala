package apps.app142

import cs214.webapp.*
import cs214.webapp.Action
import cs214.webapp.utils.WebappSuite
import os.truncate
import scala.util.{Try, Success, Failure}

class Tests extends WebappSuite[Event, State, View]:
  val sm: Logic = new Logic()

  def dicesSize(state: State): Int =
    sm.project(state)(UID0).stateView.assertInstanceOf[StateView.Playing].diceView.size


  /** Projects a given state for each given player and extract the [[state]]
    * field of the result.
    */ 
    // -------> Done
  def projectPlayingViews(userIds: Seq[UserId])(state: State) =
    USER_IDS
      .map(sm.project(state))
      .map(_.stateView.assertInstanceOf[StateView.Playing])

  /** Projects a given state for each given player and extracts the
    * [[scoreView]].
    */
    // -----> Done
  def projectScoresViews(userIds: Seq[UserId])(state: State) =
    userIds
      .map(sm.project(state))
      .map(_.scoresView)

  case class TurnResult(actions: Seq[Action[State]]):
    def viewingDicesState = 
      assert(actions.nonEmpty)
      actions.head.assertInstanceOf[Action.Render[State]].st

    def nextRoundStartState = 
      assert(actions.nonEmpty)
      actions.last.assertInstanceOf[Action.Render[State]].st

    def allStates = Seq(viewingDicesState, nextRoundStartState)

  def rollDice(state: State): State =
    val result = assertSuccess: 
      sm.transition(state)(state.players.head, Event.ButtonClicked(ButtonType.Roll))
    val newState = result.last.asInstanceOf[Action.Render[State]].st
    if newState.phase == Phase.SelectingDice then
      assert(result.length >= 28)
    //* Skull end */
    else 
      assert(result.length == 32)
    newState    


  def selectDices(state: State, selectedDice: Set[DiceId]): State =
    var newState = state
    for diceId <- selectedDice do
      newState = assertSingleRender:
        sm.transition(newState)(UID0, Event.DiceClicked(diceId))
    newState

  def endTurn(states: State): State =
    val result = assertSuccess: 
      sm.transition(states)(states.players.head, Event.ButtonClicked(ButtonType.End))
    result.last.asInstanceOf[Action.Render[State]].st

  def playOneTurn(initState: State,selectedDices: Set[DiceId]): List[State] =
    var states = List(initState)
    states = rollDice(states.head) :: states
    if states.head.phase == Phase.SelectingDice then
      states = selectDices(states.head, selectedDices) :: states
      states = rollDice(states.head) :: states  
      if states.head.phase == Phase.SelectingDice then
        states = endTurn(states.head) :: states
        states.reverse
      else
      //** Skull end -> next player will start her turn*/
        assert(states.head.phase == Phase.StartingTurn)
        states.reverse
    else 
      //** Skull end -> next player will start her turn*/
      assert(states.head.phase == Phase.StartingTurn)
      return states.reverse
    


/// # Unit tests

/// ## Initial state

/// ## Fixed mock state

  //Let's create a known state that is going to be used to test accurately the different option of the game 
  //Using a pre-made state allow us to know the symbol of each dice depending on its index
  //To do so we are going to fix the seed of the game to always have the same result
  //After some visual tests, we were able to determine the different case to allow us to test the different option of the game
  val seed = 42 //Let's fix the seed of the game to 42
  lazy val initState = sm.initSeed(USER_IDS, Some(seed))
  //The actions of the game must be executed once and the same variable should be used for the rest of the game to ensure that the seed holds for all the states
  lazy val firstRollState = rollDice(initState)

  def determineDiceNature(dice: Dice): Unit =
    Dice.All.foreach { typ =>
      assert(dice != typ, s"Assertion failed: dice is a $typ")
    }

  test("MS: Testing initial configuration"):
    val idxDice = 0
    //determineDiceNature(firstRollState.dices(idxDice))

  // ## Seed 42 configuration
  //Initial configuration of the dices with the seed 42
  //These configuration are determined using 
  protected val initialDiceConfig = Vector(Dice.Coin, Dice.Sword, Dice.Skull, Dice.Coin, Dice.Skull, Dice.Diamond, Dice.Parrot, Dice.Coin)
  protected val initialDiceViewConfig = initialDiceConfig.map(dice => if dice == Dice.Skull then DiceView.NonClickable(dice)  else DiceView.Unselected(dice))
  
  test("MS: Initial state has all dices empty"):
    val views = projectPlayingViews(USER_IDS)(initState)

    for view <- views do
      assertEquals(view.diceView.size, 8)
      for dice <- view.diceView do
        assertEquals(dice, DiceView.NonClickable(Dice.Empty))
  
  test("MS: Initial state has non-clickable"):
    val views = projectPlayingViews(USER_IDS)(initState)
    for view <- views do
      assert(view.diceView.forall({
        case DiceView.NonClickable(dice) => dice == Dice.Empty
        case _ => false
      }))

  test("MS: Initial state has all players at score 0"):
    val scoresForEachPlayer = projectScoresViews(USER_IDS)(initState)
    for matchedDice <- scoresForEachPlayer do
      assertEquals(matchedDice.keys.toSet, USER_IDS.toSet)
      for userId <- USER_IDS do
        assertEquals(matchedDice(userId), 0)

/// ##Rerolling dice state
  test("MS: Clicking on the roll button should randomize all the dice"):
    val newState =firstRollState  
    //We must verify that after rerolling the dices, the dices are not empty
    for view <- projectPlayingViews(USER_IDS)(newState) do
      view.diceView.foreach(diceView => assert(diceView != DiceView.NonClickable(Dice.Empty)))

  test("MS: Clicking on the roll button should change the dice to the expected seed configuration"):
    //For this seed, we have done a visual verification using the UI to determine the dices configuration
    val newState = firstRollState
    //We must verify that after rerolling the dices, the dices are not empty
    for view <- projectPlayingViews(USER_IDS)(newState) do
      //The view from the active player is different from the other waiting players
      if view.currentPlayer == newState.players.head then 
        for diceIdx <- 0 to 7 do 
            assertEquals(view.diceView(diceIdx), initialDiceViewConfig(diceIdx), s"Assertion failed at dice index $diceIdx")
      else 
        view.diceView.foreach(dice => assert(dice.isInstanceOf[DiceView.NonClickable]))

  test("MS: Starting state should forbid the player from clicking on end button"):
    assertFailure[IllegalMoveException]:
      sm.transition(initState)(UID0, Event.ButtonClicked(ButtonType.End))

  test("MS: Starting state should forbid the player from clicking on a dice"):
    for diceIdx <- 0 to 7 do 
      assertFailure[IllegalMoveException]:
        sm.transition(initState)(UID0, Event.DiceClicked(diceIdx))

/// ## Selecting dice state

  test("MS: Playing state should let the player chose one dice and mark it as selected"):
    val initialState = firstRollState
    //This id was determined after visual tests and represents a dice with symbol ........
    val selectedDice = Set(3)
    val newState = selectDices(initialState, selectedDice)

    for view <- projectPlayingViews(USER_IDS)(newState) do
      if view.currentPlayer == newState.players.head then
        assert((view.diceView.zipWithIndex.forall((diceV, idx) => 
          if diceV.getDice == Dice.Skull then (diceV.isInstanceOf[DiceView.NonClickable])
          else if selectedDice.contains(idx) then (diceV.isInstanceOf[DiceView.Selected])
          else (diceV.isInstanceOf[DiceView.Unselected]))))
      else 
        assert(view.diceView.forall(diceV => diceV.isInstanceOf[DiceView.NonClickable]))

  test("MS: Playing state should let the player chose three dice and mark them as selected"):
    val initialState = firstRollState
    //This id was determined after visual tests and represents a dice with symbol ........
    val selectedDice = Set(3,5,6)
    val newState = selectDices(initialState, selectedDice)

    for view <- projectPlayingViews(USER_IDS)(newState) do
      if view.currentPlayer == newState.players.head then
        assert((view.diceView.zipWithIndex.forall((diceV, idx) => 
          if diceV.getDice == Dice.Skull then (diceV.isInstanceOf[DiceView.NonClickable])
          else if selectedDice.contains(idx) then (diceV.isInstanceOf[DiceView.Selected])
          else (diceV.isInstanceOf[DiceView.Unselected]))))
      else 
        assert(view.diceView.forall(diceV => diceV.isInstanceOf[DiceView.NonClickable]))

  test("MS: Playing state should forbid the player from choosing one skull dice"):
    val initialState = firstRollState
    //This id was determined after visual tests and represents a dice with a skull symbol
    val selectedDiceIdx = 4
    assertFailure[IllegalMoveException]:
      sm.transition(initialState)(UID0, Event.DiceClicked(selectedDiceIdx))

  test("MS: Playing state should forbid the player from rerolling dice if non are selected"):
    val initialState = firstRollState
    assertFailure[IllegalMoveException]:
      sm.transition(initialState)(UID0, Event.ButtonClicked(ButtonType.Roll))

/// ## End of round state

  test("MS: If current player roll dice and save them directly the next player should then be able to play"):
    val initialState = firstRollState
    val result = assertSuccess: 
      sm.transition(initialState)(UID0, Event.ButtonClicked(ButtonType.End))

    //Verify that the saving hand is well triggered
    val savingEnd = result.head.asInstanceOf[Action.Render[State]].st
    for view <- projectPlayingViews(USER_IDS)(savingEnd) do
      if view.currentPlayer == savingEnd.players.head then
        assert(view.phase == PhaseView.SavingEnd)
      else 
        assert(view.phase == PhaseView.Waiting)

    // Ensure the next player turn is well triggered
    val newTurnState = result.last.asInstanceOf[Action.Render[State]].st
    for view <- projectPlayingViews(USER_IDS)(newTurnState) do
      if view.currentPlayer == newTurnState.players.head then
        assert(view.phase == PhaseView.Starting)
      else 
        assert(view.phase == PhaseView.Waiting)
    val currentPlayer = projectPlayingViews(USER_IDS)(newTurnState)(1).currentPlayer
    assert(newTurnState.phase == Phase.StartingTurn, newTurnState.phase.toString())
    assertEquals(currentPlayer, UID1)

  
  test("MS: After playing one round the next player should be able to start her turn"):
    val state = playOneTurn(initState, Set(1)).last
    assert(state.players.head == initState.players.tail.head)
    assert(state.phase == Phase.StartingTurn)

  
// ##End of game
//   test("Logic: IllegalMoveException is thrown when phase is EndingGame") {
//   val state = State(players = Seq(), phase = Phase.EndingGame, dices = List(), selectedDices = Set(), score = Map(), seed = 42)
//   val exception = intercept[IllegalMoveException] {
//     sm.transition(state)(UID0, Event.ButtonClicked(ButtonType.Roll))
//   }
//   assertEquals(exception.getMessage, "Game is over !")
// }

  /**
  test("APP142: When all players have chosen their hand, hands are shown, there is a pause and next round starts (2pts)"):
    val roundRes = playOneRound(initState, USER_IDS)

    // Three actions: reveal hands, wait, start next round
    assertEquals(roundRes.actions.length, 3)

    // Assert that we reveal the hands
    for
      uid <- USER_IDS
      view = sm.project(roundRes.viewingHandsState)(uid).phaseView
    do
      view.assertInstanceOf[PhaseView.ViewingHands]

    // Check that we have a proper pause
    val Action.Pause(durationMs) = roundRes.actions(1).assertInstanceOf[Action.Pause[State]]
    assert(durationMs > 100, "Too fast!")

    // Assert that next round starts
    for
      uid <- USER_IDS
      view = sm.project(roundRes.nextRoundStartState)(uid).phaseView
    do
      view.assertInstanceOf[PhaseView.SelectingHand]

  test("APP142: At the end of a round, the state should contain the correct hands"):
    val lastState = playOneRound(initState, USER_IDS).viewingHandsState
    for
      uid <- USER_IDS
      view = sm.project(lastState)(uid).phaseView.assertInstanceOf[PhaseView.ViewingHands]
    do
      assertEquals(view, PhaseView.ViewingHands(hands = gameHands))

  test("APP142: At the beginning of next round, the users should be non-ready"):
    val lastState = playOneRound(initState, USER_IDS).nextRoundStartState
    for
      uid <- USER_IDS
      view = sm.project(lastState)(uid).phaseView.assertInstanceOf[PhaseView.SelectingHand]
    do
      assertEquals(view, PhaseView.SelectingHand(ready = USER_IDS.map(_ -> false).toMap))

  test("APP142: At the beginning of next round, the state should contain the correct scores"):
    val lastState = playOneRound(initState, USER_IDS).nextRoundStartState
    val scores = gameHands.map((uid, hand) =>
      uid -> gameHands.foldLeft(0)((score, userHand) =>
        score + hand.scoreAgainst(userHand._2)
      )
    )
    for
      uid <- USER_IDS
      view = sm.project(lastState)(uid)
    do
      assertEquals(view.scoresView, scores)
  */

/// ## Additional tests

  test("MS: The game should work with different subsets of players"):
    for
      n <- 1 to USER_IDS.length
      c :Seq[UserId] <- USER_IDS.combinations(n)
    do
      val state = sm.initSeed(USER_IDS, Some(seed))
      val selectedDices = Set(1)
      val states = playOneTurn(state, Set(1))
      assert(states.length > 1)


  test("MS: The number of dices should not change from round to round") {
    val nDices = dicesSize(initState)
    for s <-  playOneTurn(initState,Set(6)) do
      assertEquals(dicesSize(s), nDices)
  }

/// ## Encoding and decoding
  
  test("MS: Different views are not equal"):
    val v1 = View(StateView.Finished(UID0, UID0), Map())
    val v2 = View(StateView.Finished(UID1, UID0), Map())
    assertNotEquals(v1, v2)
  
  test("MS: Different events of dice clicked are not equal"):
    val e1 = Event.DiceClicked(1)
    val e2 = Event.DiceClicked(2)
    assertNotEquals(e1, e2)
  
  test("MS: Different events of dice clicked are not equal"):
    val e1 = Event.ButtonClicked(ButtonType.Roll)
    val e2 = Event.ButtonClicked(ButtonType.End)
    assertNotEquals(e1, e2)
  
  test("MS: Dice event wire"):
    for diceId <- 0 to 8 do 
      Event.DiceClicked(diceId).testEventWire
  
  test("MS: Button event wire"):
    Event.ButtonClicked(ButtonType.Roll).testEventWire
    Event.ButtonClicked(ButtonType.End).testEventWire

  test("MS: View wire - Starting view with one unselected dice and one clickable button"):
    val stateView = StateView.Playing(PhaseView.Starting, UID0, Vector(DiceView.Unselected(Dice.Empty)), Vector(ButtonView.Clickable(Button.Roll)))
    val scoreView = Map(UID0 -> 0, UID1 -> 0, UID2 -> 0)
    val view = View(stateView, scoreView)
    view.testViewWire
  
  test("MS: View wire - Selecting Dice view with 3 dice (selected, unselected, nonclickable) and one clickable button"):
    val dicesView = Vector(DiceView.Selected(Dice.Empty), DiceView.Unselected(Dice.Empty), DiceView.NonClickable(Dice.Empty))
    val stateView = StateView.Playing(PhaseView.SelectingDice, UID0, dicesView, Vector(ButtonView.Clickable(Button.Roll)))
    val scoreView = Map(UID0 -> 0, UID1 -> 0, UID2 -> 0)
    val view = View(stateView, scoreView)
    view.testViewWire

  test("MS: View wire - ViewingDice view with one selected dice and one clickable and one nonclickable buttons"):
    val buttonView = Vector(ButtonView.Clickable(Button.Roll),ButtonView.NonClickable(Button.End))
    val stateView = StateView.Playing(PhaseView.ViewingDice, UID0, Vector(DiceView.Selected(Dice.Empty)),buttonView)
    val scoreView = Map(UID0 -> 0, UID1 -> 0, UID2 -> 0)
    val view = View(stateView, scoreView)
    view.testViewWire

  test("MS: View wire - SkullEnd view with 8 unselected dices and one clickable button"):
    val dices = List.fill(8)(Dice.Empty).toVector
    val diceView = dices.map(dice => DiceView.Unselected(dice) ).toVector
    val stateView = StateView.Playing(PhaseView.SkullEnd, UID0, diceView, Vector(ButtonView.Clickable(Button.Roll)))
    val scoreView = Map(UID0 -> 0, UID1 -> 0, UID2 -> 0)
    val view = View(stateView, scoreView)
    view.testViewWire

  test("MS: View wire - SavingEnd view with 8 unselected dices and two clickable buttons"):
    val dices = List.fill(8)(Dice.Empty).toVector
    val diceView = dices.map(dice => DiceView.NonClickable(dice) ).toVector
    val buttonView = Vector(ButtonView.Clickable(Button.Roll),ButtonView.Clickable(Button.End))
    val stateView = StateView.Playing(PhaseView.SavingEnd, UID0, diceView, buttonView)
    val scoreView = Map(UID0 -> 0, UID1 -> 0, UID2 -> 0)
    val view = View(stateView, scoreView)
    view.testViewWire

  test("MS: View wire - Waiting view with 8 selected non-empty dices and two clickable buttons"):
    val dices = List.fill(8)(Dice.Sword).toVector
    val diceView = dices.map(dice => DiceView.Selected(dice) ).toVector
    val buttonView = Vector(ButtonView.Clickable(Button.Roll),ButtonView.Clickable(Button.End))
    val stateView = StateView.Playing(PhaseView.Waiting, UID0, diceView, buttonView)
    val scoreView = Map(UID0 -> 0, UID1 -> 0, UID2 -> 0)
    val view = View(stateView, scoreView)
    view.testViewWire

  test("MS: View wire - init state"):
    sm.project(initState)(UID0).testViewWire

  test("MS: View wire"):
    for
      n <- 1 to USER_IDS.length
      userIds = USER_IDS.take(n)
      s <- playOneTurn(sm.init(userIds), Set(1))
      u <- userIds
    do
      sm.project(s)(u).testViewWire
      