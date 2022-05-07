package kofre.primitives

import kofre.Defs
import kofre.decompose.UIJDLattice
import kofre.decompose.interfaces.EpocheInterface

case class Epoche[E](counter: Defs.Time, value: E)

object Epoche {

  given epocheAsUIJDLattice[E: UIJDLattice]: UIJDLattice[Epoche[E]] = new UIJDLattice[Epoche[E]] {
    override def lteq(left: Epoche[E], right: Epoche[E]): Boolean = (left, right) match {
      case (Epoche(cLeft, vLeft), Epoche(cRight, vRight)) =>
        cLeft < cRight || (cLeft == cRight && UIJDLattice[E].lteq(vLeft, vRight))
    }

    /** Decomposes a lattice state into ic unique irredundant join decomposition of join-irreducible states */
    override def decompose(state: Epoche[E]): Iterable[Epoche[E]] = state match {
      case Epoche(c, v) =>
        UIJDLattice[E].decompose(v).map(Epoche(c, _))
    }

    override def empty: Epoche[E] = Epoche(0, UIJDLattice[E].empty)

    /** By assumption: associative, commutative, idempotent. */
    override def merge(left: Epoche[E], right: Epoche[E]): Epoche[E] = (left, right) match {
      case (Epoche(cLeft, vLeft), Epoche(cRight, vRight)) =>
        if (cLeft > cRight) left
        else if (cRight > cLeft) right
        else Epoche(cLeft, UIJDLattice[E].merge(vLeft, vRight))
    }
  }
}
