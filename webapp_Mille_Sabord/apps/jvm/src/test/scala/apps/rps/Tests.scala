package apps.rps

import cs214.webapp.*
import cs214.webapp.Action
import cs214.webapp.utils.WebappSuite

class Tests extends WebappSuite[Event, State, View]:
  val sm = Logic()

  /** Projects a given state for each given player and extract the [[phaseView]]
    * field of the result.
    */
  def projectSelectingHandViews(userIds: Seq[UserId])(state: State) =
    USER_IDS
      .map(sm.project(state))
      .map(_.phaseView.assertInstanceOf[PhaseView.SelectingHand])

  /** Projects a given state for each given player and extracts the
    * [[scoreView]].
    */
  def projectScoresViews(userIds: Seq[UserId])(state: State) =
    userIds
      .map(sm.project(state))
      .map(_.scoresView)

  case class RoundResult(actions: Seq[Action[State]]):
    def viewingHandsState =
      assert(actions.nonEmpty)
      actions.head.assertInstanceOf[Action.Render[State]].st

    def nextRoundStartState =
      assert(actions.nonEmpty)
      actions.last.assertInstanceOf[Action.Render[State]].st

    def allStates =
      Seq(viewingHandsState, nextRoundStartState)

/// # Unit tests

/// ## Initial state

  lazy val initState = sm.init(USER_IDS)

  test("RPS: Initial state has all players not ready (2pts)"):
    val views = projectSelectingHandViews(USER_IDS)(initState)
    for view <- views do
      view.ready.forall(!_._2)

  test("RPS: Initial state has all players at score 0 (2pts)"):
    val scoresForEachPlayer = projectScoresViews(USER_IDS)(initState)
    for score <- scoresForEachPlayer do
      score.forall(_._2 == 0)

/// ## Selecting hands state

  test("RPS: Selecting hands state should let the player select a hand and mark them as ready (4pts)"):
    val newState = assertSingleRender:
      sm.transition(initState)(UID0, Event.HandSelected(Hand.Rock))

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

  test("RPS: The game should work with different subsets of players (1pt)"):
    for
      n <- 1 to USER_IDS.length
      c <- USER_IDS.combinations(n)
    do
      playOneRound(sm.init(c), c)

  test("RPS: Hands are cyclic (none is better) (1pt)"):
    assertEquals(
      (for
        hand1 <- Hand.allHands
        hand2 <- Hand.allHands
      yield hand1.scoreAgainst(hand2))
        .sum,
      0
    )

/// ## Encoding and decoding

  test("RPS: Different views are not equal (0pt)"):
    val v1 = View(PhaseView.SelectingHand(Map(UID0 -> false)), Map())
    val v2 = View(PhaseView.SelectingHand(Map(UID0 -> true)), Map())
    assertNotEquals(v1, v2)

  test("RPS: Different events are not equal (0pt)"):
    val e1 = Event.HandSelected(Hand.Rock)
    val e2 = Event.HandSelected(Hand.Paper)
    assertNotEquals(e1, e2)

  test("RPS: Event wire (2pt)"):
    for hand <- Hand.allHands do
      Event.HandSelected(hand).testEventWire

  test("RPS: View wire (8pts)"):
    for
      n <- 1 to USER_IDS.length
      userIds = USER_IDS.take(n)
      s <- playOneRound(sm.init(userIds), userIds).allStates
      u <- userIds
    do
      sm.project(s)(u).testViewWire
