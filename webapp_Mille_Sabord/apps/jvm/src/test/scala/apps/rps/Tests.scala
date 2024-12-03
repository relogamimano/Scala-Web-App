package apps.rps

import cs214.webapp.*
import cs214.webapp.Action
import cs214.webapp.utils.WebappSuite
import os.truncate

class Tests extends WebappSuite[Event, State, View]:
  val sm = Logic()

  def dicesSize(state: State): Int =
    sm.project(state)(UID0).state.assertInstanceOf[StateView.Playing].dices.size


  /** Projects a given state for each given player and extract the [[state]]
    * field of the result.
    */ 
    // -------> Done
  def projectPlayingViews(userIds: Seq[UserId])(state: State) =
    USER_IDS
      .map(sm.project(state))
      .map(_.state.assertInstanceOf[StateView.Playing])


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

      def nextRoundStartState = assert(actions.nonEmpty)
      actions.last.assertInstanceOf[Action.Render[State]].st

    def allStates = Seq(viewingHandsState, nextRoundStartState)

/// # Unit tests

/// ## Initial state

  lazy val initState = sm.init(USER_IDS)
  
  test("MS: Initial state has all dices empty"):
    val views = projectPlayingViews(USER_IDS)(initState)

    for view <- views do
      assertEquals(view.dices.size, 8)
      for dice <- view.diceView do
        assertEquals(dice, DiceView.NonClickable(Dice.Empty))
  
  test("MS: Initial state has non-clickable"):
    val views = projectPlayingViews(USER_IDS)(initState)
    for view <- views do
      view.diceView.forall(
        dice match 
          case DiceView.Selectable(d) => false
          case DiceView.Unselected(d) => false
          case DiceView.NonClickable(d) => true
        )

  test("MS: Initial state has all players at score 0"):
    val scoresForEachPlayer = projectScoresViews(USER_IDS)(initState)
    for score <- scoresForEachPlayer do
      score.forall(_._2 == 0)

/// ##Rerolling dice state
  test("MS: Clicking on the roll button should randomize all the dice"):
    val newState = assertSingleRender:
      sm.transition(initState)(UID0, Event.ButtonClicked(ButtonType.Roll))

    for view <- projectPlayingViews(USER_IDS)(newState) do
      for dice <- view.diceView do 
        assert(dice )


/// ## Selecting hands state

  test("RPS: Selecting hands state should let the player select a hand and mark them as ready (4pts)"):
    val newState = assertSingleRender:
      sm.transition(initState)(UID0, Event.DiceClicked(1))

    for view <- projectSelectingHandViews(USER_IDS)(newState) do
      assert(view.ready(UID0))

  test("RPS: Selecting hands state should forbid the player from selecting more than one hand (2pts)"):
    val stateWithOneSelectedHand = assertSingleRender:
      sm.transition(initState)(UID0, Event.HandSelected(Hand.Rock))
    assertFailure[IllegalMoveException]:
      sm.transition(stateWithOneSelectedHand)(UID0, Event.HandSelected(Hand.Paper))

/// ## End of round state

  val gameHands: Map[UserId, Hand] =
    USER_IDS
      .tail
      .map(_ -> Hand.Rock)
      .toMap + (USER_IDS.head -> Hand.Paper)

  def playOneRound(initState: State, userIds: Seq[UserId]) =
    var state = initState
    for uid <- userIds.tail do
      state = assertSingleRender:
        sm.transition(state)(uid, Event.HandSelected(gameHands(uid)))
    RoundResult(assertMultipleActions(
      sm.transition(state)(userIds.head, Event.HandSelected(gameHands(userIds.head))),
      3
    ))

  test("RPS: When all players have chosen their hand, hands are shown, there is a pause and next round starts (2pts)"):
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

  test("RPS: At the end of a round, the state should contain the correct hands"):
    val lastState = playOneRound(initState, USER_IDS).viewingHandsState
    for
      uid <- USER_IDS
      view = sm.project(lastState)(uid).phaseView.assertInstanceOf[PhaseView.ViewingHands]
    do
      assertEquals(view, PhaseView.ViewingHands(hands = gameHands))

  test("RPS: At the beginning of next round, the users should be non-ready"):
    val lastState = playOneRound(initState, USER_IDS).nextRoundStartState
    for
      uid <- USER_IDS
      view = sm.project(lastState)(uid).phaseView.assertInstanceOf[PhaseView.SelectingHand]
    do
      assertEquals(view, PhaseView.SelectingHand(ready = USER_IDS.map(_ -> false).toMap))

  test("RPS: At the beginning of next round, the state should contain the correct scores"):
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

/// ## Additional tests

  test("RPS: The game should work with different subsets of players"):
    for
      n <- 1 to USER_IDS.length
      c <- USER_IDS.combinations(n)
    do
      playOneRound(sm.init(c), c)

  test("MS: The number of dices should not change from round to round") {
    val nDices = dicesSize(initState)
    for s <- playOneRound(initState) do
      assertEquals(dicesSize(s), nCards)
      assertEquals(dicesSize(s), nCards)
  }

/// ## Encoding and decoding
  
  test("MS: Different views are not equal"):
    val v1 = View(StateView.Finished(UID0), Map())
    val v2 = View(StateView.Finished(UID1), Map())
    assertNotEquals(v1, v2)
  
  test("MS: Different events of dice clicked are not equal"):
    val e1 = Event.DiceClicked(1)
    val e2 = Event.DiceClicked(2)
    assertNotEquals(e1, e2)
  
  test("MS: Different events of dice clicked are not equal"):
    val e1 = Event.ButtonClicked(ButtonId.Roll)
    val e2 = Event.ButtonClicked(ButtonId.End)
    assertNotEquals(e1, e2)
  
  test("MS: Dice event wire"):
    for diceId <- 0 to 8 do 
      Event.DiceClicked(diceId).testEventWire
  
  test("MS: Button event wire"):
    Event.ButtonClicked(ButtonId.Roll).testEventWire
    Event.ButtonClicked(ButtonId.End).testEventWire
  
  test("MS: View wire"):
    for
      n <- 1 to USER_IDS.length
      userIds = USER_IDS.take(n)
      s <- playOneRound(sm.init(userIds), userIds).allStates
      u <- userIds
    do
      sm.project(s)(u).testViewWire
