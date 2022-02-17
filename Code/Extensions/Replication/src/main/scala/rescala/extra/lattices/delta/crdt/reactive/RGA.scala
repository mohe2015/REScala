package rescala.extra.lattices.delta.crdt.reactive

import kofre.decompose.interfaces.RGAInterface
import kofre.decompose.interfaces.RGAInterface.{RGACompanion, State}
import kofre.decompose.{Delta, UIJDLattice}

/** [[ReactiveCRDT Reactive]] implementation of [[rescala.extra.lattices.delta.interfaces.RGAInterface RGAInterface]]
  *
  * Instead of the class constructor, you should use the apply method of the companion object to create new instances.
  *
  * @tparam E Type of the elements stored in the list
  * @tparam C Type of the causal context used for this causal CRDT
  */
class RGA[E](
    val state: State[E],
    val replicaID: String,
    val deltaBuffer: List[Delta[State[E]]]
) extends RGAInterface[E, RGA[E]] with ReactiveCRDT[State[E], RGA[E]] {

  override protected def copy(state: State[E], deltaBuffer: List[Delta[State[E]]]): RGA[E] =
    new RGA(state, replicaID, deltaBuffer)
}

object RGA extends RGACompanion {

  /** Creates a new RGA instance
    *
    * @param replicaID Unique id of the replica that this instance is located on
    * @tparam E Type of the elements stored in the list
    * @tparam C Type of the causal context used for this causal CRDT
    */
  def apply[E](replicaID: String): RGA[E] =
    new RGA(UIJDLattice[State[E]].bottom, replicaID, List())
}
