package kofre.decompose.interfaces

import kofre.base.DecomposeLattice
import kofre.time.Dots
import kofre.decompose.*
import kofre.syntax.OpsSyntaxHelper
import kofre.time.Dot
import kofre.decompose.interfaces.LWWRegisterInterface.LWWRegister
import kofre.dotted.{DotFun, Dotted}

/** An MVRegister (Multi-Value Register) is a Delta CRDT modeling a register.
  *
  * In the absence of concurrent writes, the MVRegister is either empty or holds one value.
  * When multiple values are written concurrently, reading the MVRegister returns a set holding all these values.
  */
object MVRegisterInterface {
  def empty[A]: MVRegister[A] = DotFun.empty

  type MVRegister[A] = DotFun[A]

  implicit class MVRegisterSyntax[C, A](container: C) extends OpsSyntaxHelper[C, MVRegister[A]](container) {

    def read(using QueryP): Set[A] = current.values.toSet

    def write(v: A)(using CausalMutationP, IdentifierP): C = {
      val nextDot = context.nextDot(replicaID)

      Dotted(
        DotFun(Map(nextDot -> v)),
        Dots.from(current.keySet + nextDot)
      ).mutator
    }

    def clear()(using CausalMutationP): C =
      Dotted(
        MVRegisterInterface.empty,
        Dots.from(current.keySet)
      ).mutator
  }
}
