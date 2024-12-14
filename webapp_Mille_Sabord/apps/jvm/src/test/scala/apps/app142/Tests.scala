package apps.app142

import cs214.webapp.*
import cs214.webapp.Action
import cs214.webapp.utils.WebappSuite
import os.truncate

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
    val newState = assertSingleRender:
      sm.transition(state)(UID0, Event.ButtonClicked(ButtonType.Roll))
    newState

  def selectDices(state: State, selectedDice: Set[DiceId]): State =
    var newState = state
    for diceId <- selectedDice do
      newState = assertSingleRender:
        sm.transition(newState)(UID0, Event.DiceClicked(diceId))
    newState


/// # Unit tests

/// ## Initial state

/// ## Fixed mock state

  //Let's create a known state that is going to be used to test accurately the different option of the game 
  //Using a pre-made state allow us to know the symbol of each dice depending on its index
  //To do so we are going to fix the seed of the game to always have the same result
  //After some visual tests, we were able to determine the different case to allow us to test the different option of the game
  val seed = 42 //Let's fix the seed of the game to 42
  lazy val initState = sm.initSeed(USER_IDS, Some(seed))
  //lazy val initState = sm.init(USER_IDS)  

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
    val newState = rollDice(initState)
    //We must verify that after rerolling the dices, the dices are not empty
    for view <- projectPlayingViews(USER_IDS)(newState) do
      assert(view.diceView.forall({
        case DiceView.Unselected(dice) => dice != Dice.Empty
        case _ => false
      })) 

  test("MS: Starting state should forbid the player from clicking on end button"):
    val newState = assertSingleRender:
      sm.transition(initState)(UID0, Event.ButtonClicked(ButtonType.End))
    assertFailure[IllegalMoveException]:
      sm.transition(newState)(UID0, Event.ButtonClicked(ButtonType.End))

  test("MS: Starting state should forbid the player from clicking on a dice"):
    for diceIdx <- 0 to 7 do 
      val newState = assertSingleRender:
        sm.transition(initState)(UID0, Event.DiceClicked(diceIdx))
      assertFailure[IllegalMoveException]:
        sm.transition(newState)(UID0, Event.DiceClicked(diceIdx))

/// ## Selecting dice state

  test("MS: Playing state should let the player chose one dice and mark it as selected"):
    val initialState = rollDice(initState)
    //This id was determined after visual tests and represents a dice with symbol ........
    val selectedDice = Set(3)
    val newState = selectDices(initialState, selectedDice)

    for view <- projectPlayingViews(USER_IDS)(newState) do
      assert((view.diceView.zipWithIndex.forall((diceV, idx) => 
        if diceV.getDice == Dice.Skull then (diceV.isInstanceOf[DiceView.NonClickable])
        else if selectedDice.contains(idx) then (diceV.isInstanceOf[DiceView.Selected])
        else (diceV.isInstanceOf[DiceView.Unselected]))))

  test("MS: Playing state should let the player chose three dice and mark them as selected"):
    val initialState = rollDice(initState)
    //This id was determined after visual tests and represents a dice with symbol ........
    val selectedDice = Set(3,5,6)
    val newState = selectDices(initialState, selectedDice)

    for view <- projectPlayingViews(USER_IDS)(newState) do
      assert((view.diceView.zipWithIndex.forall((diceV, idx) => 
        if diceV.getDice == Dice.Skull then (diceV.isInstanceOf[DiceView.NonClickable])
        else if selectedDice.contains(idx) then (diceV.isInstanceOf[DiceView.Selected])
        else (diceV.isInstanceOf[DiceView.Unselected]))))

  test("MS: Playing state should forbid the player from choosing one skull dice"):
    val initialState = rollDice(initState)
    //This id was determined after visual tests and represents a dice with a skull symbol
    val selectedDiceIdx = 4
    val newState = selectDices(initialState, Set(selectedDiceIdx))
    println(newState.dices(selectedDiceIdx))
    assert(newState.dices(selectedDiceIdx) == Dice.Skull)

    assertFailure[IllegalMoveException]:
      sm.transition(initialState)(UID0, Event.DiceClicked(selectedDiceIdx))

  test("MS: Playing state should forbid the player from rerolling dice if non are selected"):
    val initialState = rollDice(initState)
    //This id was determined after visual tests and represents a dice with a skull symbol
    val newState = rollDice(initialState)

    assertFailure[IllegalMoveException]:
      sm.transition(newState)(UID0, Event.ButtonClicked(ButtonType.Roll))

/// ## End of round state

  def playOneTurn(initState: State, players: Seq[UserId], selectedDices: Set[DiceId]) =
    var state = initState
    state = rollDice(state)
    state = selectDices(state, selectedDices)
    state = rollDice(state)
    state = assertSingleRender:
      sm.transition(state)(players.head, Event.ButtonClicked(ButtonType.End))
    TurnResult(assertMultipleActions(sm.transition(state)(players.head, Event.ButtonClicked(ButtonType.End)), 3 + selectedDices.size))


  test("MS: If current player roll dice and save them directly the next player should then be able to play"):
    val initialState = rollDice(initState)
    val newState = assertSingleRender:
      sm.transition(initialState)(UID0, Event.ButtonClicked(ButtonType.End))
    val view = sm.project(newState)(UID0).stateView.assertInstanceOf[StateView.Playing]
      
    val currentPlayer = projectPlayingViews(USER_IDS)(newState)(0).currentPlayer
    assertEquals(currentPlayer, UID1)
    
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
      playOneTurn(sm.init(c), c, Set(4))

  test("MS: The number of dices should not change from round to round") {
    val nDices = dicesSize(initState)
    for s <-  playOneTurn(initState,USER_IDS,Set(6)).allStates do
      assertEquals(dicesSize(s), nDices)
      assertEquals(dicesSize(s), nDices )
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
      s <- playOneTurn(sm.init(userIds), userIds, Set(2)).allStates
      u <- userIds
    do
      sm.project(s)(u).testViewWire