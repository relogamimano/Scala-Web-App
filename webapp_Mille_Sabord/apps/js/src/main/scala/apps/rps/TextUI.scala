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

  val handNames = Map(
    "rock" -> Hand.Rock,
    "r" -> Hand.Rock,
    "paper" -> Hand.Paper,
    "p" -> Hand.Paper,
    "scissors" -> Hand.Scissors,
    "s" -> Hand.Scissors
  )

  override def handleTextInput(view: View, text: String): Option[Event] = handNames
    .get(text.toLowerCase())
    .map(Event.HandSelected.apply)

  override def renderView(userId: UserId, view: View): Vector[TextSegment] =
    Vector(
      TextSegment(text = "Rock, Paper, Scissors\n\n", modifiers = cls := "title")
    ) ++ renderView(view)

  def renderView(view: View): Vector[TextSegment] =
    phaseView(view.phaseView) ++ scoresView(view.scoresView)

  def phaseView(phaseView: PhaseView): Vector[TextSegment] = phaseView match
    case PhaseView.SelectingHand(ready) =>
      val notReadyPlayers = ready.filterNot(_._2).map(_._1)
      (Vector(
        TextSegment(
          if notReadyPlayers.isEmpty then ""
          else
            s"${notReadyPlayers.mkString(",")} " +
              s"${if notReadyPlayers.size > 1 then "are" else "is"} " +
              "choosing their next hand\n"
        )
      ) ++ renderPlayerChoices(ready.map((userId, isReady) => (userId, if isReady then "✅" else "💭")))
        :+ TextSegment("\n"))
        ++ (if !ready(userId) then renderHandButtons(userId) else Vector.empty)
    case PhaseView.ViewingHands(hands) =>
      TextSegment("Here are the results!\n") +: renderPlayerChoices(hands)

  def renderPlayerChoices(hands: Map[UserId, String]): Vector[TextSegment] =
    Vector(
      TextSegment(hands.map { case (userId, hand) => s"$userId: $hand" }.mkString("", ", ", "\n"))
    )

  def renderHandButtons(userId: UserId): Vector[TextSegment] =
    TextSegment("Choose your hand: ")
      +: Hand.allHands.toSeq.map { hand =>
        TextSegment(
          hand,
          onMouseEvent = Some({
            case MouseEvent.Click(_) => sendEvent(Event.HandSelected(hand))
            case _                   => ()
          }),
          modifiers = cls := "clickable"
        )
      }.toVector
      :+ TextSegment("\n\n")

  def scoresView(scoresView: ScoresView): Vector[TextSegment] =
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
      |   font-size: 150%;
      |   letter-spacing: 1.5em;
      | }
  """.stripMargin
