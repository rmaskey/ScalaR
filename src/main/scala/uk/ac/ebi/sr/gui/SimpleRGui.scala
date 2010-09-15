/*
 * Copyright (c) 2009-2010 European Molecular Biology Laboratory
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */
package uk.ac.ebi.sr
package gui

import swing.event.ButtonClicked
import swing._
import GridBagPanel.Anchor
import javax.swing.UIManager
import java.awt.{Insets, Point, Font}
import uk.ac.ebi.sr.model.Environment

/**
 *
 * Date: 20.05.2010
 * @author Taalai Djumabaev
 */
@deprecated
object SimpleRGui extends SimpleSwingApplication {
  try {
    UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName());
  } catch { case e: Exception => }
  val fontName = "Serif"
  val fontSize = 16
  val fontStyle = Font.HANGING_BASELINE
  val inputFieldColumnNumber = 80
  val resultAreaColumnNumber = inputFieldColumnNumber / 2

  val resultAreaRowNumber = 15
  val frameLocation = new Point(100, 200)

  var rExecutor = new RExecutor(Environment.emptyEnv)

  def top = new MainFrame {
    title = "Scala simple RGUI"

    val promtLabel = new Label {
      text = ">"
    }
    val resultsArea = new ResultTextArea("results:\n")
    val treeArea = new ResultTextArea("parse tree:\n")
    val execute = new Button("execute")
    val inputField = new InputTextField

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
        contents += inputField
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
    listenTo(inputField)
    reactions += {
      case ButtonClicked(`execute`) => {
        val parseResult = rExecutor.parseAndFormat(inputField.text.split(";")(0))
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