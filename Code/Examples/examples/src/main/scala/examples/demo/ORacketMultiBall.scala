package examples.demo

import java.awt.Color

import examples.demo.LFullyModularBall.BouncingBall
import examples.demo.MPlayingFieldBall.PlayingField
import examples.demo.ui.{Rectangle, Shape, ShapesPanel}
import rescala.default._

/** Lastly, we implement a Racket module, that implements a Rectangle
  * positioned on either the left or right side of the field, and moving
  * vertically to a desired position, but bound by the playing field height.
  * We instantiate one for the left side, controlled by the Mouse.y position,
  * and add it to the list of displayed shapes. To also support multiple
  * balls, we again implement its collision computation as blueprint
  * derivations inside a method. We add an according instantiation into
  * the ball initialization closure, adding the collision event as a
  * horizontal bounce source.
  */
object ORacketMultiBall extends Main {
  class Racket(
      val fieldWidth: Signal[Int],
      val isRight: Boolean,
      val fieldHeight: Signal[Int],
      val inputY: Signal[Int]
  ) {
    val height = Var(100)
    val width  = Var(10)

    val posX = fieldWidth.map(w => (if (isRight) 1 else -1) * (w / 2 - 25))
    val posY = {
      Signal {
        math.max(
          math.min(
            inputY.value,
            (fieldHeight.value - height.value) / 2
          ),
          -(fieldHeight.value - height.value) / 2
        )
      }
    }

    def collisionWith(collider: Shape): Event[Unit] = {
      val collisionBoxHeight = Signal { height() + collider.hitboxHeight() }
      val collisionBoxWidth  = Signal { width() + collider.hitboxWidth() }
      val shapeInsideRacket = Signal {
        (posX() - collisionBoxWidth() / 2 < collider.centerX()) &&
        (posX() + collisionBoxWidth() / 2 > collider.centerX()) &&
        (posY() - collisionBoxHeight() / 2 < collider.centerY()) &&
        (posY() + collisionBoxHeight() / 2 > collider.centerY())
      }
      shapeInsideRacket.changedTo(true)
    }

    val shape = new Rectangle(posX, posY, width, height, fill = Var(Some(if (isRight) Color.BLUE else Color.RED)))
  }

  val shapes = Var[List[Shape]](List.empty)
  val panel  = new ShapesPanel(shapes)

  val playingField = new PlayingField(panel.width.map(_ - 25), panel.height.map(_ - 25))
  val racket       = new Racket(playingField.width, true, playingField.height, panel.Mouse.y)
  shapes.transform(playingField.shape :: racket.shape :: _)

  val balls = List(
    new BouncingBall(200d, 150d, Var(50), panel.Mouse.middleButton.pressed),
    new BouncingBall(-200d, 100d, Var(50), panel.Mouse.middleButton.pressed)
  )

  for (bouncingBall <- balls) {
    shapes.transform(bouncingBall.shape :: _)

    val fieldCollisions = playingField.colliders(bouncingBall.shape)
    bouncingBall.horizontalBounceSources.transform(fieldCollisions.left :: fieldCollisions.right :: _)
    bouncingBall.verticalBounceSources.transform(fieldCollisions.top :: fieldCollisions.bottom :: _)

    val racketCollision = racket.collisionWith(bouncingBall.shape)
    bouncingBall.horizontalBounceSources.transform(racketCollision :: _)
  }
}
