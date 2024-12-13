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
    "💀" -> Dice.Skull,
    "diamond" -> Dice.Diamond,
    "💎" -> Dice.Diamond,
    "coin" -> Dice.Coin,
    "📀" -> Dice.Coin,
    "sword" -> Dice.Sword,
    "🔪" -> Dice.Sword,
    "monkey" -> Dice.Monkey,
    "🐵" -> Dice.Monkey,
    "parrot" -> Dice.Parrot,
    "🐦" -> Dice.Parrot,
    "empty" -> Dice.Empty,
    "❓" -> Dice.Empty
  )

  val buttonNames = Map(
    "roll" -> ButtonType.Roll,
    "roll the dice" -> ButtonType.Roll,
    "end" -> ButtonType.End,
    "end my turn" -> ButtonType.End
  )

  override def handleTextInput(view: View, text: String): Option[Event] = 
    diceNames.get(text.toLowerCase()) match {
      case Some(dice) => view.stateView match {
        case StateView.Playing(_, _, diceView, _) =>
          diceView.zipWithIndex.collectFirst {
            case (DiceView.Unselected(`dice`), diceID) =>
              Event.DiceClicked(diceID)
            case (DiceView.Selected(`dice`), diceID) =>
              Event.DiceClicked(diceID)
          }
        case _ => None
      }
      case None => 
        buttonNames.get(text.toLowerCase()) match {
          case Some(buttonId) => 
            Some(Event.ButtonClicked(buttonId))
          case None => 
            None
        }
    }


  override def renderView(userId: UserId, view: View): Vector[TextSegment] =
    Vector(
      TextSegment(text = "Mille Sabords\n\n", modifiers = cls := "title")
    ) ++ renderPage(userId, view)

  def renderPage(userId: UserId, view: View): Vector[TextSegment] =
    renderState(userId, view.stateView) ++ 
    Vector(
      TextSegment("\n", modifiers = cls := "gap")  // Add a gap between the state and scores
    ) ++ renderScores(view.scoresView) ++
    Vector(
      TextSegment("\n", modifiers = cls := "gap")  // Add a gap between the state and scores
    ) ++ {
      view.stateView match {
        case StateView.Playing(_, _, _, _) => renderFooter()
        case _ => Vector.empty
      }
    }

  def renderState(userId: UserId, stateView: StateView): Vector[TextSegment] = stateView match
    case StateView.Playing(phase, currentPlayer, diceView, buttonView) =>
      Vector(
        TextSegment(s"Current player: $currentPlayer\n", modifiers = cls := "bold"),
        TextSegment("\n", modifiers = cls := "gap")  // Add a gap after the current player
      ) ++ renderPhase(phase) ++ 
      Vector(
        TextSegment("\n", modifiers = cls := "gap")  // Add a gap before the dice
      ) ++ renderDice(diceView) ++ 
      Vector(
        TextSegment("\n", modifiers = cls := "gap")  // Add a gap before the buttons
      ) ++ renderButtons(buttonView) ++ 
      Vector(
        TextSegment("\n", modifiers = cls := "gap")  // Add a gap before the scores
      )

    case StateView.Finished(winnerId, currentPlayer) =>
      val headerSegments = if (winnerId == currentPlayer) {
        Vector(
          TextSegment(
            "You Win!!! 🥳\n",
            modifiers = cls := "win-header",
          )
        )
      } else {
        Vector(
          TextSegment(
            "You Lose... 😭\n",
            modifiers = cls := "lose-header",
          )
        )
      }

      val messageSegments = Vector(
        TextSegment(s"The game is over! Winner: $winnerId\n", modifiers = cls := "italic")
      )

      headerSegments ++ Vector(TextSegment("\n", modifiers = cls := "gap")) ++ messageSegments


  def renderPhase(phase: PhaseView): Vector[TextSegment] = phase match
    case PhaseView.Starting =>
      Vector(TextSegment("Start your turn and roll the dice!\n"))
    case PhaseView.SelectingDice =>
      Vector(TextSegment("Select the dice you want to rethrow or end your turn:\n"))
    case PhaseView.ViewingDice =>
      Vector(TextSegment("Here's what you got! How many points do you think you have?\n"))
    case PhaseView.SkullEnd =>
      Vector(TextSegment("Shoot! You got 3 skulls. Game over :(\n"))
    case PhaseView.SavingEnd =>
      Vector(TextSegment("Here's your score for this turn!\n"))
    case PhaseView.Waiting =>
      Vector(TextSegment("Let's watch your opponent play...\n"))

  def renderDice(diceView: Vector[DiceView]): Vector[TextSegment] = {
    val (firstRow, secondRow) = diceView.splitAt(diceView.size / 2)
    
    val renderRow = (row: Vector[DiceView], startIdx: Int) =>
      row.zipWithIndex.map { case (dice, idx) =>
        val diceID = startIdx + idx
        dice match {
          case DiceView.Selected(dice) =>
            TextSegment(
              s"[Selected: $dice] ",
              onMouseEvent = Some({
                case MouseEvent.Click(_) =>
                  sendEvent(Event.DiceClicked(diceID))
                case _ => ()
              }),
              modifiers = cls := "clickable"
            )
          case DiceView.Unselected(dice) =>
            TextSegment(
              s"[$dice] ",
              onMouseEvent = Some({
                case MouseEvent.Click(_) =>
                  sendEvent(Event.DiceClicked(diceID))
                case _ => ()
              }),
              modifiers = cls := "clickable"
            )
          case DiceView.NonClickable(dice) =>
            val diceString = dice match {
              case Dice.Skull => "Skull:"
              case Dice.Empty => "Empty:"
              case _          => ""
            }
            TextSegment(s"[$diceString $dice] ", modifiers = cls := "skull")
        }
      } ++ Vector(TextSegment("\n"))

    renderRow(firstRow, 0) ++ renderRow(secondRow, firstRow.size)
  }

  def renderButtons(buttonView: Vector[ButtonView]): Vector[TextSegment] = {
    val buttonRow = buttonView.map {
      case ButtonView.Clickable(button) =>
        TextSegment(
          s"[$button] ",
          onMouseEvent = Some({
            case MouseEvent.Click(_) =>
              sendEvent(
                if button == Button.Roll then Event.ButtonClicked(ButtonType.Roll)
                else Event.ButtonClicked(ButtonType.End)
              )
            case _ => ()
          }),
          modifiers = cls := "clickable"
        )
      case ButtonView.NonClickable(button) =>
        TextSegment(s"[$button] ", modifiers = cls := "non-clickable")
    }
    buttonRow ++ Vector(TextSegment("\n", modifiers = cls := "gap"))
  }

  def renderScores(scoresView: ScoresView): Vector[TextSegment] = {
    Vector(
      TextSegment("Scores:\n", modifiers = cls := "bold")
    ) ++ scoresView.map { case (userId, score) =>
      TextSegment(s"$userId: $score\n", modifiers = cls := "score")
    }
  }

  
  def renderFooter(): Vector[TextSegment] =
    Vector(
      TextSegment("\n", modifiers = cls := "gap"),
      TextSegment("Score sheet:\n", modifiers = cls := "bold underline"),
      TextSegment("3 x 💀 ......   End with 0\n"),
      TextSegment("any 📀 ......   100\n"),
      TextSegment("any 💎 ......   100\n"),
      TextSegment("3 x 🔲 ......   100\n"),
      TextSegment("4 x 🔲 ......   200\n"),
      TextSegment("5 x 🔲 ......   500\n"),
      TextSegment("6 x 🔲 ......   1000\n"),
      TextSegment("7 x 🔲 ......   2000\n"),
      TextSegment("8 x 🔲 ......   4000\n"),
      TextSegment("\n", modifiers = cls := "gap")
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
      | .gap {
      |   margin-top: 10px 0;  /* Vertical spacing */
      | }
      | .score {
      |   margin-bottom: 0.5em; /* Vertical spacing for scores */
      | }
      | .win-header {
      |   color: green;
      |   font-size: 150%;
      |   font-weight: bold;
      | }
      |
      | .lose-header {
      |   color: red;
      |   font-size: 150%;
      |   font-weight: bold;
      | }
      |
      | .italic {
      |   font-style: italic;
      | }
    """.stripMargin