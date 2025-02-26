package reswing

import scala.swing.{Color, Dimension, Font, TextComponent}
import scala.swing.event.{CaretUpdate, ValueChanged}

class ReTextComponent(
    val text: ReSwingValue[String] = (),
    val editable: ReSwingValue[Boolean] = (),
    `caret.position`: ReSwingValue[Int] = (),
    `caret.markDot`: ReSwingValue[(Int, Int)] = (),
    `caret.visible`: ReSwingValue[Boolean] = (),
    `caret.selectionVisible`: ReSwingValue[Boolean] = (),
    `caret.blinkRate`: ReSwingValue[Int] = (),
    `caret.color`: ReSwingValue[Color] = (),
    cut: ReSwingEvent[Unit] = (),
    copy: ReSwingEvent[Unit] = (),
    paste: ReSwingEvent[Unit] = (),
    selectAll: ReSwingEvent[Unit] = (),
    background: ReSwingValue[Color] = (),
    foreground: ReSwingValue[Color] = (),
    font: ReSwingValue[Font] = (),
    enabled: ReSwingValue[Boolean] = (),
    minimumSize: ReSwingValue[Dimension] = (),
    maximumSize: ReSwingValue[Dimension] = (),
    preferredSize: ReSwingValue[Dimension] = ()
) extends ReComponent(background, foreground, font, enabled, minimumSize, maximumSize, preferredSize) {
  override protected lazy val peer: TextComponent with ComponentMixin = new TextComponent with ComponentMixin
  final lazy val localPeer: TextComponent with ComponentMixin         = peer

  val selected = ReSwingValue.using({ () => peer.selected }, (caret.peer, classOf[CaretUpdate]))

  (text.using({ () => peer.text }, peer.text_= _, classOf[ValueChanged]).force("editable", peer.editable_= _, false))

  editable.using({ () => peer.editable }, peer.editable_= _, "editable")

  cut.using(() => peer.cut())
  copy.using(() => peer.copy())
  paste.using(() => peer.paste())
  selectAll.using(() => peer.selectAll())

  class ReCaret(
      val position: ReSwingValue[Int],
      val markDot: ReSwingValue[(Int, Int)],
      val visible: ReSwingValue[Boolean],
      val selectionVisible: ReSwingValue[Boolean],
      val blinkRate: ReSwingValue[Int],
      val color: ReSwingValue[Color]
  ) {
    protected[ReTextComponent] val peer: ReTextComponent.this.localPeer.caret.type =
      ReTextComponent.this.localPeer.caret

    val dot  = ReSwingValue.using({ () => peer.dot }, (peer, classOf[CaretUpdate]))
    val mark = ReSwingValue.using({ () => peer.mark }, (peer, classOf[CaretUpdate]))

    position.using({ () => peer.position }, peer.position_= _, (peer, classOf[CaretUpdate]))

    markDot.using(
      { () => (peer.mark, peer.dot) },
      { _ match { case (mark, dot) => peer.position = mark; peer.moveDot(dot) } },
      (peer, classOf[CaretUpdate])
    )

    visible.using({ () => peer.visible }, peer.visible_= _)
    selectionVisible.using({ () => peer.selectionVisible }, peer.selectionVisible_= _)
    blinkRate.using({ () => peer.blinkRate }, peer.blinkRate_= _)
    color.using({ () => peer.color }, peer.color_= _)
  }

  object ReCaret {
    implicit def toCaret(caret: ReCaret): caret.peer.type = caret.peer
  }

  object caret
      extends ReCaret(
        `caret.position`,
        `caret.markDot`,
        `caret.visible`,
        `caret.selectionVisible`,
        `caret.blinkRate`,
        `caret.color`
      )
}

object ReTextComponent {
  implicit def toTextComponent(component: ReTextComponent): TextComponent = component.peer
}
