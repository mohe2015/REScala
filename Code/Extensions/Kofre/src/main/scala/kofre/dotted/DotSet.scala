package kofre.dotted

import kofre.time.{Dot, Dots}
import kofre.dotted.DottedDecompose.FromConlattice
import kofre.datatypes.EnableWinsFlag
import kofre.decompose.interfaces

case class DotSet(repr: Dots) {
  def dots: Dots = repr
  export repr.*
}

object DotSet {

  def empty: DotSet = DotSet(Dots.empty)

  def from(it: IterableOnce[Dot]): DotSet = DotSet(Dots.from(it))

  given hasDots: HasDots[DotSet] with {
    override def dots(a: DotSet): Dots = a.repr
  }

  /** This essentially tracks the currently present dots, and all dots */
  given dottedLattice: DottedLattice[DotSet] with {
    override def mergePartial(left: Dotted[DotSet], right: Dotted[DotSet]): DotSet = {
      val fromLeft  = left.store.repr subtract right.context
      val fromRight = right.store.repr.subtract(left.context subtract left.store.repr)

      DotSet(fromLeft union fromRight)
    }
  }

  /** DotSet is a dot store implementation that is simply a set of dots. See [[EnableWinsFlag]] for a
    * usage example.
    */
  given contextDecompose: DottedDecompose[DotSet] =
    new FromConlattice[DotSet](dottedLattice) {

      override def lteq(left: Dotted[DotSet], right: Dotted[DotSet]): Boolean = {
        val firstCondition = left.context.forall(right.context.contains)

        val secondCondition = {
          val diff = left.context.diff(left.store.repr)
          right.store.repr.intersect(diff).isEmpty
        }

        firstCondition && secondCondition
      }

      override def decompose(state: Dotted[DotSet]): Iterable[Dotted[DotSet]] = {
        val added =
          for (d <- state.store.repr.iterator) yield
            val single = DotSet(Dots.single(d))
            Dotted(single, single.repr)
        val removed = state.context.subtract(state.store.repr).decomposed.map(Dotted(DotSet.empty, _))
        removed ++ added
      }
    }

}
