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
      renderView(userId, view),
      
    )

  def renderView(userId: UserId, view: View): Frag =
    frag(
      renderState(userId, view.stateView),
      renderScores(view.scoresView),
      view.stateView match {
        case StateView.Playing(_, _, _, _) => renderFooter()
        case _ => frag()
      }
    )

  def renderState(userId: UserId, stateView: StateView): Frag = stateView match
    case StateView.Playing(phase, currentPlayer, diceView, buttonView) =>
      frag (
        p(b(s"Current player: "), s"$currentPlayer"),
        renderPhase(phase),
        renderDice(diceView),
        renderButtons(buttonView)
      )
    case StateView.Finished(winnerId, currentPlayer) =>
      frag(
        // Conditionally render the header based on winnerId and currentPlayer
        if (winnerId == currentPlayer) {
          h1(
            cls := "win-header",
            style := "color: green;", // Green for win
            "You Win!!! 🥳"
          )
        } else {
          h1(
            cls := "lose-header",
            style := "color: red;", // Red for lose
            "You Lose... 😭 "
          )
        },
        // Display the general game over message
        p(i(s"The game is over! Winner: $winnerId"))
      )


  def renderScores(scores: Map[UserId, Int]) = frag(
    p(b(s"Scores:")),
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
      frag(p(cls := "centered-text", "Start your turn and roll the dice!"))
    case PhaseView.SelectingDice => 
      frag(p(cls := "centered-text", "Select the dice you want to rethrow or end your turn:"))
    case PhaseView.ViewingDice => 
      frag(p(cls := "centered-text", "Here's what you got! How many points do you think you have?"))
    case PhaseView.SkullEnd => 
      frag(p(cls := "centered-text", "Shoot! You got 3 skulls. Game over :("))
    case PhaseView.SavingEnd => 
      frag(p(cls := "centered-text", "Here's your score for this turn!"))
    case PhaseView.Waiting =>
      frag(p(cls := "centered-text", "Let's watch your opponent play..."))

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
      cls := "dice-container-wrapper", // Wrapper for centering
      div(
        cls := "dice-container",
        for (dice, diceID) <- diceView.zipWithIndex yield dice match
          case DiceView.Selected(dice) =>
            div(
              cls := "dice selected",
              onclick := { () => 
                val diceEvent = Event.DiceClicked(diceID) 
                sendEvent(diceEvent)
              },
              dice
            )

          case DiceView.Unselected(dice) =>
            div(
              cls := "dice",
              onclick := { () =>
                val diceEvent = Event.DiceClicked(diceID)
                sendEvent(diceEvent)
              },
              dice
            )

          case DiceView.NonClickable(dice) =>
            div(cls := "dice skull", dice)
      )
    )

  
  def renderFooter(): Frag =
    div(
      cls:= "footer-wrapper",
      div(
        cls := "footer",
        p(u("Score sheet:")),
        div(
          cls := "cheatsheet",
          p(s"any 💎 ......   100"),
          p(s"3 x 🔲 ......   100"),
          p(s"4 x 🔲 ......   200"),
          p(s"5 x 🔲 ......   500"),
          p(s"6 x 🔲 ......   1000"),
          p(s"7 x 🔲 ......   2000"),
          p(s"8 x 🔲 ......   4000")
        )
      )
    )
    

  override def css: String = super.css +
    """| .dice-container {
      |   display: grid;
      |   grid-template-columns: repeat(4, 1fr);
      |   grid-template-rows: repeat(2, 1fr);
      |   gap: 1rem;
      |   margin-bottom: 2rem; /* Add gap between dice and buttons */
      | }
      | .dice-container-wrapper {
      |   display: flex;
      |   justify-content: center;
      |   align-items: center;
      |   height: 100%; /* Ensure it takes up full height of parent */
      | }
      | .dice {
      |   width: 80px; /* Increased size */
      |   height: 80px; /* Increased size */
      |   font-size: 2.5rem;
      |   text-align: center;
      |   border: 2px solid black; /* Default border thickness */
      |   border-radius: 5px;
      |   display: flex;
      |   justify-content: center;
      |   align-items: center;
      | }
      | .selected {
      |   border-color: green;
      |   border-width: 4px; /* Thicker border when selected */
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
      | .centered-text {
      |   text-align: center; /* Center align text */
      |   font-style: normal; /* Remove italics */
      | }
      | .cheatsheet {
      |   background-color: #f9e3a1; 
      |   padding: 20px;
      |   width: 300px; /* Adjust width as needed */
      |   border-radius: 10px;
      |   box-shadow: 0px 4px 8px rgba(0, 0, 0, 0.1); /* Soft shadow */
      |   font-family: "Courier New", monospace; /* Fixed-width font for alignment */
      |   font-size: 18px;
      |   margin-top: 1rem;
      | }
      | .cheatsheet p {
      |   margin: 10px 0;
      |   white-space: nowrap; /* Prevents wrapping and ensures alignment */
      |   text-align: left; /* Aligns the text to the left */
      | }
      | .footer {
      |   margin-top: 2rem;
      |   text-align: center;
      | }
      | .footer-wrapper {
      |   display: flex;
      |   justify-content: center;
      |   align-items: flex-start; /* Aligns content to the top, if needed */
      |   height: 100%; /* Ensure it takes up full height of parent */
      | }
    """.stripMargin
