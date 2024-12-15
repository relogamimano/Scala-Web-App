package apps
package app142

import cs214.webapp.*
import cs214.webapp.client.*
import cs214.webapp.client.graphics.*
import scalatags.JsDom.all.*

import scala.scalajs.js.annotation.JSExportTopLevel

@JSExportTopLevel("app142_text")
object TextUI extends WSClientApp:
  def appId: String = "app142"
  def uiId: String = "text"

  def init(userId: UserId, sendMessage: ujson.Value => Unit, target: Target): ClientAppInstance =
    TextUIInstance(userId, sendMessage, target)

class TextUIInstance(userId: UserId, sendMessage: ujson.Value => Unit, target: Target)
    extends graphics.TextClientAppInstance[Event, View](userId, sendMessage, target):

  override val wire = app142.Wire

  val diceNames = Map(
    // Skull
    "select skull" -> ("select", Dice.Skull),
    "deselect skull" -> ("deselect", Dice.Skull),
    "select 💀" -> ("select", Dice.Skull),
    "deselect 💀" -> ("deselect", Dice.Skull),
    
    // Diamond
    "select diamond" -> ("select", Dice.Diamond),
    "deselect diamond" -> ("deselect", Dice.Diamond),
    "select 💎" -> ("select", Dice.Diamond),
    "deselect 💎" -> ("deselect", Dice.Diamond),
    
    // Coin
    "select coin" -> ("select", Dice.Coin),
    "deselect coin" -> ("deselect", Dice.Coin),
    "select 🪙" -> ("select", Dice.Coin),
    "deselect 🪙" -> ("deselect", Dice.Coin),
    
    // Sword
    "select sword" -> ("select", Dice.Sword),
    "deselect sword" -> ("deselect", Dice.Sword),
    "select 🔪" -> ("select", Dice.Sword),
    "deselect 🔪" -> ("deselect", Dice.Sword),
    
    // Monkey
    "select monkey" -> ("select", Dice.Monkey),
    "deselect monkey" -> ("deselect", Dice.Monkey),
    "select 🐵" -> ("select", Dice.Monkey),
    "deselect 🐵" -> ("deselect", Dice.Monkey),
    
    // Parrot
    "select parrot" -> ("select", Dice.Parrot),
    "deselect parrot" -> ("deselect", Dice.Parrot),
    "select 🐦" -> ("select", Dice.Parrot),
    "deselect 🐦" -> ("deselect", Dice.Parrot),
    
    // Empty
    "select empty" -> ("select", Dice.Empty),
    "deselect empty" -> ("deselect", Dice.Empty),
    "select ❓" -> ("select", Dice.Empty),
    "deselect ❓" -> ("deselect", Dice.Empty)
  )


  val buttonNames = Map(
    "roll" -> ButtonType.Roll,
    "roll the dice" -> ButtonType.Roll,
    "end" -> ButtonType.End,
    "end my turn" -> ButtonType.End
  )

  override def handleTextInput(view: View, text: String): Option[Event] = 
    diceNames.get(text.toLowerCase()) match {
      case Some(("select", dice)) => view.stateView match {
        case StateView.Playing(_, _, diceView, _) =>
          diceView.zipWithIndex.collectFirst {
            case (DiceView.Unselected(`dice`), diceID) =>
              Event.DiceClicked(diceID) // Select the first unselected dice
          }
        case _ => None
      }

      case Some(("deselect", dice)) => view.stateView match {
        case StateView.Playing(_, _, diceView, _) =>
          diceView.zipWithIndex.collectFirst {
            case (DiceView.Selected(`dice`), diceID) =>
              Event.DiceClicked(diceID) // Deselect the first selected dice
          }
        case _ => None
      }
      case Some(_,_) => None
      case None => 
        buttonNames.get(text.toLowerCase()) match {
          case Some(buttonId) => Some(Event.ButtonClicked(buttonId))
          case None => None
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
      Vector(TextSegment("Select the dice you want to rethrow or end your turn:\n"),
      TextSegment("(Select/deselect skull/💀, diamond/💎, coin/🪙, sword/🔪, monkey/🐵, parrot/🐦)\n", modifiers = cls := "small"))
    case PhaseView.ViewingDice =>
      Vector(TextSegment("Drumroll please... 🥁\n"))
    case PhaseView.SkullEnd =>
      Vector(TextSegment("Shoot! You have at least 3 skulls. You lose this round :(\n"))
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
      TextSegment("\n", modifiers = cls := "gap"),
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
      | .small {
      |   font-size: 0.7em; 
      | }
    """.stripMargin