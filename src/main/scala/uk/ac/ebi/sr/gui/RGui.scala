package uk.ac.ebi.sr
package gui

import swing._
import event.ButtonClicked
import GridBagPanel.Anchor
import javax.swing.UIManager
import java.awt.{Font, Insets, Point}
import uk.ac.ebi.sr.interpreter.{Environment}

/**
 * RGUI works
 *
 * Date: 19.05.2010
 * @author Taalai Djumabaev
 */

object RGui extends SimpleGUIApplication {
  try {
    UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName());
  } catch { case e: Exception => }
  val fontName = "Serif"
  val fontSize = 16
  val fontStyle = Font.HANGING_BASELINE
  val inputFieldColumnNumber = 80
  val resultAreaColumnNumber = inputFieldColumnNumber / 2

  val resultAreaRowNumber = 15
  val inputAreaRowNumber = 3
  val frameLocation = new Point(100, 200)

  var rExecutor = new RExecutor(new Environment(collection.mutable.Map(), None))

  def top = new MainFrame {
    title = "Scala simple RGUI"

    val promtLabel = new Label {
      text = ">"
    }
    val resultsArea = new ResultTextArea("results:\n")
    val treeArea = new ResultTextArea("parse tree:\n")
    val execute = new Button("execute")
    val inputArea = new TextArea {
      columns = inputFieldColumnNumber
      rows = inputAreaRowNumber
      peer.setFont(new Font(fontName, fontStyle, fontSize))
    }

    contents = new GridBagPanel {
      add(new FlowPanel {
        contents += new ScrollPane(resultsArea)
        contents += new ScrollPane(treeArea)
      }, new Constraints {
        anchor = Anchor.Center
        insets = new Insets(5, 5, 5, 5)
        gridy = 3
      })
      add(new FlowPanel {
        contents += promtLabel
        contents += new ScrollPane(inputArea)
      }, new Constraints {
        anchor = Anchor.SouthWest
        insets = new Insets(5, 5, 5, 5)
        gridy = 4
      })
      add(execute, new Constraints {
        anchor = Anchor.SouthEast
        insets = new Insets(5, 5, 10, 5)
        gridy = 4
      })
      border = Swing.EmptyBorder(20, 20, 20, 20)
    }
    location_=(frameLocation)
    listenTo(execute)
    listenTo(inputArea)
    reactions += {
      case ButtonClicked(`execute`) => {
        val parseResult = rExecutor.parseAndFormat(inputArea.text)
        resultsArea append parseResult._1
        treeArea append parseResult._2
      }
    }
  }

  override def main(args: Array[String]) {
    super.main(args)
  }

  class ResultTextArea(t: String) extends TextArea {
    columns = resultAreaColumnNumber
    rows = resultAreaRowNumber
    text = t
    peer.setEditable(false)
    peer.setFont(new Font(fontName, fontStyle, fontSize))
  }

  class InputTextField extends TextField {
    columns = inputFieldColumnNumber
    peer.setFont(new Font(fontName, fontStyle, fontSize))
  }
}