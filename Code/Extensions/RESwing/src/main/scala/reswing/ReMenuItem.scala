package reswing

import javax.swing.Icon

import scala.swing.{Action, Alignment, Color, Dimension, Font, MenuItem}

class ReMenuItem(
    text: ReSwingValue[String] = (),
    action: Action = null,
    selected: ReSwingValue[Boolean] = (),
    horizontalAlignment: ReSwingValue[Alignment.Value] = (),
    verticalAlignment: ReSwingValue[Alignment.Value] = (),
    horizontalTextPosition: ReSwingValue[Alignment.Value] = (),
    verticalTextPosition: ReSwingValue[Alignment.Value] = (),
    icon: ReSwingValue[Icon] = (),
    pressedIcon: ReSwingValue[Icon] = (),
    selectedIcon: ReSwingValue[Icon] = (),
    disabledIcon: ReSwingValue[Icon] = (),
    disabledSelectedIcon: ReSwingValue[Icon] = (),
    rolloverIcon: ReSwingValue[Icon] = (),
    rolloverSelectedIcon: ReSwingValue[Icon] = (),
    background: ReSwingValue[Color] = (),
    foreground: ReSwingValue[Color] = (),
    font: ReSwingValue[Font] = (),
    enabled: ReSwingValue[Boolean] = (),
    minimumSize: ReSwingValue[Dimension] = (),
    maximumSize: ReSwingValue[Dimension] = (),
    preferredSize: ReSwingValue[Dimension] = ()
) extends ReAbstractButton(
      text,
      selected,
      horizontalAlignment,
      verticalAlignment,
      horizontalTextPosition,
      verticalTextPosition,
      icon,
      pressedIcon,
      selectedIcon,
      disabledIcon,
      disabledSelectedIcon,
      rolloverIcon,
      rolloverSelectedIcon,
      background,
      foreground,
      font,
      enabled,
      minimumSize,
      maximumSize,
      preferredSize
    ) {
  override protected lazy val peer: MenuItem with ComponentMixin = new MenuItem(null: String) with ComponentMixin

  if (action != null)
    peer.action = action
}

object ReMenuItem {
  implicit def toMenuItem(component: ReMenuItem): MenuItem = component.peer
}
