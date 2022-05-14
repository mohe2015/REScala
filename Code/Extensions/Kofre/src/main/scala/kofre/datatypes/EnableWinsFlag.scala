package kofre.datatypes

import kofre.base.{Bottom, DecomposeLattice}
import kofre.datatypes.Epoche
import kofre.decompose.*
import kofre.dotted.{DottedDecompose, DotSet, Dotted, HasDots}
import kofre.syntax.OpsSyntaxHelper
import kofre.time.Dots

/** An EWFlag (Enable-Wins Flag) is a Delta CRDT modeling a boolean flag.
  *
  * When the flag is concurrently disabled and enabled then the enable operation wins, i.e. the resulting flag is enabled.
  */
case class EnableWinsFlag(inner: DotSet)

object EnableWinsFlag {

  given contextDecompose: DottedDecompose[EnableWinsFlag] = DottedDecompose.derived
  given hasDotsEWF: HasDots[EnableWinsFlag] with {
    override def dots(a: EnableWinsFlag): Dots = a.inner.repr
  }

  val empty: EnableWinsFlag = EnableWinsFlag(DotSet.empty)

  /** It is enabled if there is a value in the store.
    * It relies on the external context to track removals.
    */
  implicit class EnableWinsFlagOps[C](container: C) extends OpsSyntaxHelper[C, EnableWinsFlag](container) {
    def read(using QueryP): Boolean = !current.inner.repr.isEmpty

    def enable()(using CausalMutationP, IdentifierP): C = {
      val nextDot = context.nextDot(replicaID)
      Dotted(
        EnableWinsFlag(DotSet(Dots.single(nextDot))),
        current.inner.repr add nextDot
        ).mutator
    }
    def disable()(using CausalMutationP): C = {
      Dotted(
        EnableWinsFlag(DotSet(Dots.empty)),
        current.inner.repr
        ).mutator
    }
  }

}
