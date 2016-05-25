package examples.bouncing


import java.awt.{Dimension, Graphics2D, Point}

import rescala._

import scala.swing.{MainFrame, Panel, SimpleSwingApplication, Swing}

object EventVersion extends SimpleSwingApplication {
  lazy val application = new EventVersion
  def top = application.frame

  override def main(args: Array[String]): Unit = {
    super.main(args)
    while (true) {
      Swing onEDTWait {application.tick()}
      Thread sleep 20
    }
  }
}

class EventVersion {
  val Size = 50
  val Max_X = 600
  val Max_Y = 600
  val speed = new Point(10, 8)

  class Coord(private var _n: Int) {
    val changed = Evt[Int]
    def n: Int = _n
    def n_=(newVal: Int): Unit = {
      _n = newVal
      changed(n)
    }
  }

  val x = new Coord(20)
  val y = new Coord(20)

  def tick(): Unit = {
    x.n += speed.x
    y.n += speed.y
    hasTicked(())
  }

  val hasTicked = Evt[Unit] // Can be afterExec

  // handle bouncing
  val xBounce = x.changed && (x => x < 0 || x + Size > Max_X)
  val yBounce = y.changed && (y => y < 0 || y + Size > Max_Y)
  xBounce += { _ => speed.x = -speed.x }
  yBounce += { _ => speed.y = -speed.y }

  // handle repaint
  hasTicked += { _ => frame.repaint }


  // drawing code
  val frame = new MainFrame {
    contents = new Panel() {
      preferredSize = new Dimension(600, 600)
      override def paintComponent(g: Graphics2D): Unit = {
        g.fillOval(x.n, y.n, Size, Size)
      }
    }
  }
}
