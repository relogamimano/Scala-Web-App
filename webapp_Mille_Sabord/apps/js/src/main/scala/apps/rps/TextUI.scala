package apps
package rps

import cs214.webapp.*
import cs214.webapp.client.*
import cs214.webapp.client.graphics.*
import scalatags.JsDom.all.*

import scala.scalajs.js.annotation.JSExportTopLevel

@JSExportTopLevel("rps_text")
object TextUI extends WSClientApp:
  def appId: String = "rps"
  def uiId: String = "text"

  def init(userId: UserId, sendMessage: ujson.Value => Unit, target: Target): ClientAppInstance =
    TextUIInstance(userId, sendMessage, target)

class TextUIInstance(userId: UserId, sendMessage: ujson.Value => Unit, target: Target)
    extends graphics.TextClientAppInstance[Event, View](userId, sendMessage, target):

  override val wire = rps.Wire

  val diceNames = Map(
    "skull" -> Dice.Skull,
    "diamond" -> Dice.Diamond,
    "coin" -> Dice.Coin,
    "sword" -> Dice.Sword,
    "monkey" -> Dice.Monkey,
    "parrot" -> Dice.Parrot
  )

  val buttonNames = Map(
    "roll" -> ButtonId.Roll,
    "end" -> ButtonId.End
  )

  // Handle both dice selection and button actions from text input
  override def handleTextInput(view: View, text: String): Option[Event] = 
    diceNames.get(text.toLowerCase()) match {
      case Some(dice) => Some(Event.DiceClicked(dice))  // Handle dice selection via text input
      case None => buttonNames.get(text.toLowerCase()) match {
        case Some(buttonId) => Some(Event.ButtonClicked(buttonId))  // Handle button click via text input
        case None => None  // No valid input
      }
    }

  override def renderView(userId: UserId, view: View): Vector[TextSegment] =
    Vector(
      TextSegment(text = "Mille Sabords\n\n", modifiers = cls := "title")
    ) ++ renderView(userId, view)

  def renderView(userId: UserId, view: View): Vector[TextSegment] =
    renderState(userId, view.stateView) ++ scoresView(view.scoresView)

  def renderState(userId: UserId, stateView: StateView): Vector[TextSegment] = stateView match
    case StateView.Playing(phase, currentPlayer, diceView, buttonView) =>
      Vector(
        TextSegment(s"Current player: $currentPlayer\n"),
        renderPhase(phase),
        renderDice(diceView),
        renderButtons(buttonView)
      ).flatten

    case StateView.Finished(winnerId) =>
      Vector(
        TextSegment(s"The game is over! Winner: $winnerId\n")
      )

  def renderPhase(phase: PhaseView): Vector[TextSegment] = phase match
    case PhaseView.Starting =>
      Vector(TextSegment("Start your turn and roll the dice!\n"))
    case PhaseView.SelectingDice =>
      Vector(TextSegment("Select the dice you want to rethrow:\n"))
    case PhaseView.ViewingDice =>
      Vector(TextSegment("Here's what you got! Do you want to end your turn or try again?\n"))
    case PhaseView.SkullEnd =>
      Vector(TextSegment("Shoot! You got 3 skulls. Game over :(\n"))
    case PhaseView.SavingEnd =>
      Vector(TextSegment("Here's your score for this turn!\n"))
    case PhaseView.Waiting =>
      Vector(TextSegment("Let's watch your opponent play...\n"))

  def renderDice(diceView: Vector[DiceView]): Vector[TextSegment] =
    diceView.map {
      case DiceView.Selected(dice) => TextSegment(s"[Selected: $dice] ")
      case DiceView.Unselected(dice) => TextSegment(s"[$dice] ")
      case DiceView.Skull(dice) => TextSegment(s"[Skull: $dice] ", modifiers = cls := "skull")
    }

  def renderButtons(buttonView: Vector[ButtonView]): Vector[TextSegment] =
    buttonView.map {
      case ButtonView.Clickable(button) =>
        TextSegment(
          s"[$button] ",
          onMouseEvent = Some({
            case MouseEvent.Click(_) =>
              sendEvent(
                if button == Button.Roll then Event.ButtonClicked(ButtonId.Roll)
                else Event.ButtonClicked(ButtonId.End)
              )
            case _ => ()
          }),
          modifiers = cls := "clickable"
        )
      case ButtonView.NonClickable(button) =>
        TextSegment(s"[$button] ", modifiers = cls := "non-clickable")
    }

  def renderScores(scoresView: ScoresView): Vector[TextSegment] =
    Vector(
      TextSegment("Scores: ", modifiers = cls := "bold"),
      TextSegment(scoresView.map { case (userId, score) => s"$userId: $score" }.mkString(", "))
    )

  override def css: String = super.css +
    """
      | .title {
      |   font-size: 120%;
      |   font-weight: bold;
      | }
      | .bold {
      |   font-weight: bold;
      | }
      | .clickable {
      |   cursor: pointer;
      |   color: green;
      | }
      | .non-clickable {
      |   color: gray;
      | }
      | .skull {
      |   color: red;
      | }
    """.stripMargin
