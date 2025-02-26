package test.kofre
import kofre.base.{Defs, Lattice}
import kofre.time.VectorClock
import org.scalacheck.{Arbitrary, Gen}
import test.kofre.DataGenerator.arbId

case class SomeProductType[A, B](paramA: A, paramB: B) derives Lattice

opaque type NotInt = Int

given Arbitrary[SomeProductType[NotInt, NotInt]] = Arbitrary(for {
  as: NotInt <- Gen.posNum[Int]
  bs: NotInt <- Gen.posNum[Int]
} yield SomeProductType(as, bs))

given Lattice[NotInt] = math.max _

class DerivedLattice extends LatticeMergeTest[SomeProductType[NotInt, NotInt]]
