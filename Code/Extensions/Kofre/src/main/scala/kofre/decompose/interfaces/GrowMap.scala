package kofre.decompose.interfaces

import kofre.base.DecomposeLattice
import kofre.time.Dots
import kofre.contextual.{AsCausalContext, ContextDecompose, ContextLattice, Dotted}
import kofre.dotted.DotMap
import kofre.syntax.{ArdtOpsContains, OpsSyntaxHelper, PermIdMutate, DottedName}

/** A GMap (Grow-only Map) is a Delta CRDT that models a map from an arbitrary key type to nested Delta CRDTs.
  * In contrast to [[ORMapInterface]], key/value pairs cannot be removed from this map. However, due to the smaller internal
  * representation, mutate operations on large maps are a lot faster than on ORMap.
  *
  * The nested CRDTs can be queried/mutated by calling the queryKey/mutateKey methods with a DeltaQuery/DeltaMutator generated
  * by a CRDT Interface method of the nested CRDT. For example, to enable a nested EWFlag, one would pass `EWFlagInterface.enable()`
  * as the DeltaMutator to mutateKey.
  */
case class GrowMap[K, V](inner: Map[K, V])

object GrowMap {

  def empty[K, V]: GrowMap[K, V] = GrowMap(Map.empty)

  given decomposeLattice[K, V: DecomposeLattice]: DecomposeLattice[GrowMap[K, V]] = DecomposeLattice.derived
  given contextLattice[K, V: ContextDecompose: AsCausalContext]: ContextDecompose[GrowMap[K, V]] =
    given ContextDecompose[Map[K, V]] = DotMap.contextDecompose[K, V].contextbimap[Map[K, V]](_.map(_.repr), _.map(DotMap.apply))
    ContextDecompose.derived

  implicit class GMapSyntax[C, K, V](container: C)(using aoc: ArdtOpsContains[C, GrowMap[K, V]])
      extends OpsSyntaxHelper[C, GrowMap[K, V]](container) {

    def contains(k: K)(using QueryP): Boolean = current.inner.contains(k)

    def queryKey(k: K)(using QueryP): Option[V] = current.inner.get(k)

    def queryAllEntries()(using QueryP): Iterable[V] = current.inner.values

    def mutateKey(k: K)(m: Option[V] => V)(using MutationIdP): C = GrowMap(Map(k -> m(queryKey(k)))).mutator

    def mutateKeyCtx(k: K)(m: PermIdMutate[V, V] => Option[V] => V)(using MutationIdP): C = {
      GrowMap(Map(k -> m(PermIdMutate.withID[V, V](replicaID))(queryKey(k)))).mutator
    }

    def mutateKeyNamedCtx(k: K, default: => V)(m: DottedName[V] => DottedName[V])(using CausalMutationP, IdentifierP): C = {
      m(Dotted(queryKey(k).getOrElse(default), context).named(replicaID)).anon.map(v => GrowMap(Map(k -> v))).mutator
    }
  }

}
