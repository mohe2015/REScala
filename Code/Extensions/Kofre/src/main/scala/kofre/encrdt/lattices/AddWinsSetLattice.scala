package kofre.encrdt.lattices
import kofre.Lattice
import kofre.Lattice.Operators
import kofre.primitives.{LamportClock, VectorClock}

case class AddWinsSetLattice[T](
    elements: Set[(T, LamportClock)] = Set[(T, LamportClock)](),
    clocks: VectorClock = VectorClock.zero
) {

  def values: Set[T] = elements.map(_._1)

  def removed(element: T): AddWinsSetLattice[T] = {
    copy(elements = elements.filterNot(_._1 == element))
  }

  def added(value: T, replicaId: String): AddWinsSetLattice[T] = {
    val clocksAfterAdd   = clocks.advance(replicaId)
    val newLocalClock    = clocksAfterAdd.clockOf(replicaId)
    val newElem          = (value, newLocalClock)
    val elementsAfterAdd = elements.filterNot(_._1 == value) + newElem
    AddWinsSetLattice(elementsAfterAdd, clocksAfterAdd)
  }

  def contains(element: T): Boolean = elements.exists(_._1 == element)
}

object AddWinsSetLattice {
  // See: https://arxiv.org/pdf/1210.3368.pdf - An Optimized Conflict-free Replicated Set
  implicit def AddWinsSetSemiLattice[T]: Lattice[AddWinsSetLattice[T]] =
    (left: AddWinsSetLattice[T], right: AddWinsSetLattice[T]) => {
      val commonElems = left.elements & right.elements

      val leftCausalElements = left.elements.filter { case (e, LamportClock(i, c)) =>
        c > right.clocks.timeOf(i)
      }

      val rightCausalElements = right.elements.filter { case (e, LamportClock(i, c)) =>
        c > left.clocks.timeOf(i)
      }

      val allElements = commonElems ++ leftCausalElements ++ rightCausalElements

      // Only keep most recent LamportClock (per replica)
      val duplicates = allElements.filter { case (e, LamportClock(c, i)) =>
        allElements.exists {
          case (`e`, LamportClock(otherC, `i`)) => c < otherC
          case _                                => false
        }
      }

      val cleanedElements = allElements -- duplicates
      val receivedSummary = left.clocks.merge(right.clocks)

      AddWinsSetLattice(cleanedElements, receivedSummary)
    }
}
