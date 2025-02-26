package kofre.decompose.interfaces

import kofre.base.DecomposeLattice
import kofre.time.Dots
import kofre.decompose.*
import kofre.syntax.OpsSyntaxHelper
import kofre.time.Dot
import kofre.dotted.{DotFun, Dotted}

/** An RCounter (Resettable Counter/Add Wins Counter) is a Delta CRDT modeling a counter.
  *
  * Calling fresh after every time that deltas are shipped to other replicas prevents subsequent increment/decrement
  * operations to be overwritten by concurrent reset operations.
  *
  * This counter was originally proposed by Baquera et al.
  * in "The problem with embedded CRDT counters and a solution", see [[https://dl.acm.org/doi/abs/10.1145/2911151.2911159?casa_token=D7n88K9dW7gAAAAA:m3WhHMFZxoCwGFk8DVoqJXBJpwJwrqKMLqtgKo_TSiwU_ErWgOZjo4UqYqDCb-bG3iJlXc_Ti7aB9w here]]
  */
object RCounterInterface {

  val zero: RCounter = DotFun.empty

  implicit def IntPairAsUIJDLattice: DecomposeLattice[(Int, Int)] = new DecomposeLattice[(Int, Int)] {
    override def lteq(left: (Int, Int), right: (Int, Int)): Boolean = (left, right) match {
      case ((linc, ldec), (rinc, rdec)) =>
        linc <= rinc && ldec <= rdec
    }

    /** Decomposes a lattice state into its unique irredundant join decomposition of join-irreducible states */
    override def decompose(state: (Int, Int)): Iterable[(Int, Int)] = state match {
      case (inc, dec) => List((inc, 0), (0, dec))
    }

    /** By assumption: associative, commutative, idempotent. */
    override def merge(left: (Int, Int), right: (Int, Int)): (Int, Int) = (left, right) match {
      case ((linc, ldec), (rinc, rdec)) =>
        (linc max rinc, ldec max rdec)
    }
  }

  type RCounter = DotFun[(Int, Int)]

  private def deltaState(
      df: Option[DotFun[(Int, Int)]] = None,
      cc: Dots
  ): Dotted[RCounter] = {
    Dotted(
      df.getOrElse(DotFun.empty),
      cc
    )
  }

  implicit class RCounterSyntax[C](container: C) extends OpsSyntaxHelper[C, RCounter](container) {

    def value(using QueryP): Int = {
      current.repr.values.foldLeft(0) {
        case (counter, (inc, dec)) => counter + inc - dec
      }
    }

    /** Without using fresh, reset wins over concurrent increments/decrements
      * When using fresh after every time deltas are shipped to other replicas, increments/decrements win over concurrent resets
      */
    def fresh()(using CausalMutationP, IdentifierP): C = {
      val nextDot = context.nextDot(replicaID)

      deltaState(
        df = Some(DotFun.empty[(Int, Int)] + (nextDot -> ((0, 0)))),
        cc = Dots.single(nextDot)
      ).mutator
    }

    private def update(u: (Int, Int))(using CausalMutationP, IdentifierP): C = {
      context.max(replicaID) match {
        case Some(currentDot) if current.repr.contains(currentDot) =>
          val newCounter = (current(currentDot), u) match {
            case ((linc, ldec), (rinc, rdec)) => (linc + rinc, ldec + rdec)
          }

          deltaState(
            df = Some(current + (currentDot -> newCounter)),
            cc = Dots.single(currentDot)
          ).mutator
        case _ =>
          val nextDot = context.nextDot(replicaID)

          deltaState(
            df = Some(DotFun.empty[(Int, Int)] + (nextDot -> u)),
            cc = Dots.single(nextDot)
          ).mutator
      }
    }

    def increment()(using CausalMutationP, IdentifierP): C = update((1, 0))

    def decrement()(using CausalMutationP, IdentifierP): C = update((0, 1))

    def reset()(using CausalMutationP): C = {
      deltaState(
        cc = Dots.from(current.keySet)
      ).mutator
    }
  }
}
