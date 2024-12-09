package apps
package rps

import PhaseView.*
import cs214.webapp.*
import cs214.webapp.client.*
import scalatags.JsDom.all
import scalatags.JsDom.all.*

import scala.scalajs.js.annotation.{JSExportAll, JSExportTopLevel}

@JSExportTopLevel("rps_html")
object HtmlUI extends WSClientApp:
  def appId: String = "rps"
  def uiId: String = "html"

  def init(userId: UserId, sendMessage: ujson.Value => Unit, target: Target): ClientAppInstance =
    HtmlUIInstance(userId, sendMessage, target)

class HtmlUIInstance(userId: UserId, sendMessage: ujson.Value => Unit, target: Target)
    extends StateMachineClientAppInstance[Event, View](userId, sendMessage, target):

  override val wire = rps.Wire

  override def render(userId: UserId, view: View): Frag =
    frag(
      h2(b("Mille Sabords: ")),
      renderView(userId, view)
    )

  def renderView(userId: UserId, view: View): Frag =
    frag(
      renderState(userId, view.stateView),
      renderScores(view.scoresView)
    )

  def renderState(userId: UserId, stateView: StateView): Frag = stateView match
    case StateView.Playing(phase, currentPlayer, diceView, buttonView) =>
      frag (
        p(i(s"Current player: $currentPlayer")),
        renderPhase(phase),
        renderDice(diceView),
        renderButtons(buttonView)
      )
    case StateView.Finished(winnerId) =>
      frag(
        p(i(s"The game is over! Winner: $winnerId"))
      )

  def renderScores(scores: Map[UserId, Int]) = frag(
    p(i(s"Scores:")),
    div(
      cls := "scores",
      for
        (player, score) <- scores.toSeq
      yield div(
        cls := "score",
        div(s"$player: $score")
      )
    )
  )

  def renderPhase(phase: PhaseView): Frag = phase match
    case PhaseView.Starting => 
      frag(p(i("Start your turn and roll the dice!")))
    case PhaseView.SelectingDice => 
      frag(p(i("Select the dice you want to rethrow:")))
    case PhaseView.ViewingDice => 
      frag(p(i("Here's what you got! Do you want to end your turn or try again?")))
    case PhaseView.SkullEnd => 
      frag(p(i("Shoot! You got 3 skulls. Game over :(")))
    case PhaseView.SavingEnd => 
      frag(p(i("Here's your score for this turn!")))
    case PhaseView.Waiting =>
      frag(p(i("Let's watch your opponent play...")))

  def renderButtons(buttonView: Vector[ButtonView]): Frag =
    div(
      cls := "buttons-container",
      for button <- buttonView yield button match
        case ButtonView.Clickable(button) =>
          div(
            cls := "button clickable",
            onclick := { () => 
              val buttonEvent = button match {
                case Button.Roll => Event.ButtonClicked(ButtonType.Roll)
                case Button.End => Event.ButtonClicked(ButtonType.End)
                // Add other button types here as needed
              }
              sendEvent(buttonEvent)
            },
            button
          )
        case ButtonView.NonClickable(button) =>
          div(cls := "button non-clickable", button)
    )


  def renderDice(diceView: Vector[DiceView]): Frag =
    div(
      cls := "dice-container",
      for dice <- diceView yield dice match
        case DiceView.Selected(dice) =>
          div(cls := "dice selected", dice)

        case DiceView.Unselected(dice) =>
          div(cls := "dice", dice)

        case DiceView.NonClickable(dice) =>
          div(cls := "dice skull", dice)
    )


  override def css: String = super.css +
    """| .dice-container {
      |   display: flex;
      |   justify-content: center;
      |   gap: 1rem;
      | }
      | .dice {
      |   width: 50px;
      |   height: 50px;
      |   font-size: 2rem;
      |   text-align: center;
      |   border: 1px solid black;
      |   border-radius: 5px;
      |   display: flex;
      |   justify-content: center;
      |   align-items: center;
      | }
      | .selected {
      |   border-color: green;
      |   background-color: #e6ffe6;
      | }
      | .skull {
      |   opacity: 0.5;
      |   background-color: gray;
      | }
      | .buttons-container {
      |   display: flex;
      |   gap: 1rem;
      |   justify-content: center;
      | }
      | .button {
      |   padding: 0.5rem 1rem;
      |   font-size: 1.2rem;
      |   border-radius: 5px;
      |   cursor: pointer;
      |   text-align: center;
      | }
      | .clickable {
      |   background-color: #4caf50;
      |   color: white;
      | }
      | .non-clickable {
      |   background-color: #d3d3d3;
      |   color: #777;
      |   cursor: not-allowed;
      | }
    """.stripMargin
