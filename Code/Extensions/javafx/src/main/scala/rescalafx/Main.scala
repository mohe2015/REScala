package rescalafx

import scalafx.application.JFXApp3.PrimaryStage
import scalafx.application.{JFXApp3, Platform}
import scalafx.scene.{Node, Scene}
import scalafx.scene.control.{Label, Slider}
import scalafx.scene.layout.VBox
import rescalafx.JFXAdapter._
import rescala.default._

object Main extends JFXApp3 {
  override def start(): Unit = {

    val slider      = new Slider(0, 30, 0)
    val sliderValue = new Label()

    sliderValue.text <== {
      val sliderSignal = slider.value.toSignal
      val str          = sliderSignal.map(v => f"$v%.2f")
      new SignalToStringProperty(str).toProperty
    }

    def render(): Node =
      new VBox {
        children = Seq(
          slider,
          sliderValue
        )
      }

    stage = new PrimaryStage {

      width = 1000
      height = 600
      title = "Slider Demo"

      scene = new Scene {
        content = render()
      }

      onCloseRequest = _ => {
        Platform.exit()
      }
    }
  }
}
