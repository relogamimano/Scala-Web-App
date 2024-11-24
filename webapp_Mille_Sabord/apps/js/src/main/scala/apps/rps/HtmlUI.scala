package apps
package rps

import PhaseView.ViewingHands
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
      h2(b("Rock paper scissors: ")),
      renderView(userId, view)
    )

  def renderView(userId: UserId, view: View): Frag =
    frag(
      renderPhase(view.phaseView),
      renderScores(view.scoresView)
    )

  def renderPhase(phaseView: PhaseView): Frag = phaseView match
    case PhaseView.SelectingHand(ready) =>
      val notReadyPlayers = ready.filterNot(_._2).map(_._1)
      frag(
        if notReadyPlayers.isEmpty then
          frag()
        else
          p(i(s"${notReadyPlayers.mkString(", ")} " +
            s"${if notReadyPlayers.size > 1 then "are" else "is"} " +
            "choosing their next hand"))
        ,
        renderPlayerChoices(ready.map((userId, isReady) => (userId, if isReady then "✅" else "💭"))),
        if !ready(userId) then
          renderHandButtons(userId, ready(userId))
        else
          frag()
      )
    case PhaseView.ViewingHands(hands) =>
      frag(
        p(i("Here are the results !")),
        renderPlayerChoices(hands)
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

  def renderPlayerChoices(playerHandStr: Map[UserId, String]) =
    div(
      cls := "choices",
      for
        (player, handStr) <- playerHandStr.toSeq
      yield div(
        div(cls := "card", handStr),
        div(player)
      )
    )

  def renderHandButtons(userId: UserId, alreadyChosen: Boolean) =
    p(
      p(i(s"Choose your hand !")),
      cls := "hands",
      if !alreadyChosen then data.interactive := "interactive" else frag(),
      for
        hand <- Hand.allHands.toSeq
      yield div(
        cls := "card",
        if !alreadyChosen then onclick := { () => sendEvent(Event.HandSelected(hand)) }
        else frag(),
        hand
      )
    )

  override def css: String = super.css +
    """| .card {
       |   aspect-ratio: 1;
       |   align-items: center;
       |   display: flex;
       |   justify-content: center;
       |   overflow: hidden;
       |   text-align: center;
       |   word-break: break-all;
       |   align-items: center;
       |   background: #eeeeec;
       |   border-radius: 0.5rem;
       |   box-sizing: border-box;
       |   cursor: default;
       |   font-size: 2.5rem;
       |   font-family: "Noto Color Emoji", "Apple Color Emoji", "Segoe UI Emoji", sans-serif;
       |   outline: thin solid var(--taupe);
       | }
       | .choices, .hands, .score {
       |     display: flex;
       |     flex-flow: row wrap;
       |     justify-content: space-evenly;
       |     align-items: center;
       | }
       | .scores {
       |     width: 100%;
       |     display: grid;
       |     gap: 0.5rem 1rem;
       |     grid-template-columns: minmax(max-content, 1rem) 1fr;
       |     align-items: baseline;
       | }
       """.stripMargin
