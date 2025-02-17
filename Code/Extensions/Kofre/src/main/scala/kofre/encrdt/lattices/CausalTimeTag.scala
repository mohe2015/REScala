package kofre.encrdt.lattices
import kofre.base.Lattice
import kofre.base.Lattice.Operators
import kofre.time.VectorClock
import kofre.encrdt.lattices.CausalTimeTag.lwwTimeOrd

import java.time.Instant
import scala.math.PartialOrdering

case class CausalTimeTag(
    vectorClock: VectorClock = VectorClock.zero,
    utc: Instant = Instant.ofEpochMilli(0),
    replicaId: String = ""
) extends Ordered[CausalTimeTag] {

  def advance(rId: String): CausalTimeTag = CausalTimeTag(vectorClock merge vectorClock.inc(rId), Instant.now(), rId)

  def toTuple: (VectorClock, Instant, String) = (vectorClock, utc, replicaId)

  override def compare(that: CausalTimeTag): Int = lwwTimeOrd.compare(this, that)
}

object CausalTimeTag {
  implicit def lwwTimeOrd: Ordering[CausalTimeTag] =
    (l, r) => Ordering[(VectorClock, Instant, String)].compare(l.toTuple, r.toTuple)

  implicit def causallyConsistentTimeReplicaOrd: Ordering[(VectorClock, Instant, String)] = (l, r) => {
    if (PartialOrdering[VectorClock].gt(l._1, r._1)) 1
    else if (PartialOrdering[VectorClock].lt(l._1, r._1)) -1
    else Ordering.by((v: (VectorClock, Instant, String)) => (v._2, v._3)).compare(l, r)
  }

  // TODO: max(vc1,vc2), max(utc1,utc2), max(rId1,rid2) would also be a plausible LUB
  given lattice: Lattice[CausalTimeTag] = (l, r) =>
    if (l > r) l
    else if (l < r) r
    else if (l == r) l
    else throw new IllegalArgumentException(s"$l and $r can't be ordered")
}
