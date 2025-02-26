package reswing.reshapes.ui.panels

import java.awt.Color

import scala.swing.Action
import scala.swing.BoxPanel
import scala.swing.Button
import scala.swing.Component
import scala.swing.FlowPanel
import scala.swing.Frame
import scala.swing.Label
import scala.swing.Orientation
import javax.swing.JColorChooser
import rescala.default._
import reswing.{ReButton, ReSlider, ReSwingValue}

/** Panel for various customization of the stroke. */
class StrokeInputPanel extends FlowPanel {
  private val colorWindow = new ColorWindow

  private val slider = new ReSlider(
    min = 1,
    max = 50,
    value = 1,
    minorTickSpacing = 1,
    labels = ReSwingValue(scala.collection.immutable.Map.empty[Int, Label]),
    paintTicks = true
  )

  private val showColorWindow = new ReButton("Show Colorinput")
  showColorWindow.clicked += { _ => colorWindow.visible = !colorWindow.visible }

  contents += new Label { text = "stroke width: " }
  contents += slider
  contents += showColorWindow

  val strokeWidth = slider.value
  val color       = colorWindow.color
}

class ColorWindow extends Frame {
  title = "Choose color"

  private object colorChooser extends Component {
    override lazy val peer: JColorChooser = new JColorChooser
  }

  contents = new BoxPanel(Orientation.Vertical) {
    contents += colorChooser
    contents += new Button(Action("OK") {
      color set colorChooser.peer.getColor
      ColorWindow.this.visible = false
    })
  }

  val color = Var(Color.BLACK) // #VAR
}
