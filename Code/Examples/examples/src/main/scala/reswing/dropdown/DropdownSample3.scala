package reswing.dropdown

import rescala.default._
import reswing.ReTextField._
import reswing._

import scala.swing._

object DropdownSample3 extends SimpleSwingApplication {

  // initial values
  val col1 = new ReTextField(text = "Berlin", columns = 30)
  val col2 = new ReTextField(text = "Paris", columns = 30)
  val val1 = Signal { col1.text() }
  val val2 = Signal { col2.text() }

  val fields: Var[List[Signal[String]]] = Var(List(val1, val2))
  val nFields                           = Signal { fields().size }

  val listOfSignals = Signal { fields() }
  val options       = Signal.dynamic { listOfSignals().map(_()) }

  val innerChanged      = Signal { listOfSignals().map(_.changed) }
  val anyChangedWrapped = Signal { innerChanged().reduce((a, b) => a || b) }
  val anyChanged        = anyChangedWrapped.flatten

  anyChanged += { x => println("some value has changed: " + x) }

  val dropdown       = new ReDynamicComboBox(options = options, selection = -1)
  val selectionIndex = Signal { dropdown.selection() }
  val validSelection = Signal { if (options().indices.contains(selectionIndex())) Some(selectionIndex()) else None }

  // select the currently selected item manually
  val currentSelectedItem = Signal.dynamic { validSelection().map { i => listOfSignals()(i)() } }
  val outputString        = Signal { currentSelectedItem().getOrElse("Nothing") }
  val outputField         = new ReTextField(text = outputString)

  object frame extends MainFrame {
    title = "Dropdown example 3"
    val fields = new BoxPanel(Orientation.Vertical) {

      contents += new FlowPanel {
        contents += new Label("Dropdown selection: ")
        contents += dropdown
      }

      contents += new FlowPanel {
        contents += new Label("Selected item: ")
        contents += outputField
      }

      contents += new FlowPanel {
        contents += Button("Add value")(addField("empty"))
      }

      contents += new FlowPanel {
        contents += new Label("Value 1:")
        contents += col1
      }

      contents += new FlowPanel {
        contents += new Label("Value 2:")
        contents += col2
      }

    }
    contents = fields
  }

  def top = frame

  def addField(initText: String): Unit = {
    val n   = nFields.readValueOnce + 1
    val col = new ReTextField(text = initText, columns = 30)
    frame.fields.contents += new FlowPanel {
      contents += new Label("Value " + n + ":")
      contents += col
    }
    val content: Signal[String] = Signal { col.text() }
    fields set content :: fields.readValueOnce
    frame.pack()
  }

}
