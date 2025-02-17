package kofre.decompose.interfaces

import kofre.datatypes.GrowMap
import kofre.decompose.*
import kofre.dotted.DottedDecompose
import kofre.syntax.{ArdtOpsContains, OpsSyntaxHelper}

/** A GSet is a Delta CRDT modeling a simple grow-only set. */
object GSetInterface {

  type GSet[E] = Set[E]

  given contextDecompose[E]: DottedDecompose[GSet[E]] = DottedDecompose.liftDecomposeLattice

  implicit class GSetSyntax[C, E](container: C) extends OpsSyntaxHelper[C, GSet[E]](container) {

    def elements(using QueryP): Set[E] = current

    def insert(element: E)(using MutationP): C = Set(element).mutator
  }
}
