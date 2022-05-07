package test.kofre

import kofre.causality.{CausalContext, Dot, VectorClock}
import kofre.contextual.WithContextDecompose
import kofre.contextual.WithContextDecompose.{DotFun, DotMap}
import kofre.primitives.{CausalQueue, LastWriterWins, MultiValueRegister}
import kofre.sets.ORSet
import kofre.{Defs, Lattice}
import org.scalacheck.{Arbitrary, Gen}

object DataGenerator {

  given arbId: Arbitrary[Defs.Id] = Arbitrary(Gen.oneOf('a' to 'g').map(_.toString))

  given arbVersion: Arbitrary[VectorClock] = Arbitrary(for {
    ids: Set[Defs.Id] <- Gen.nonEmptyListOf(arbId.arbitrary).map(_.toSet)
    value: List[Long] <- Gen.listOfN(ids.size, Gen.oneOf(0L to 100L))
  } yield VectorClock.fromMap(ids.zip(value).toMap))

  given arbLww: Arbitrary[LastWriterWins[Int]] = Arbitrary(
    for {
      time  <- Gen.long
      value <- Gen.choose(Int.MinValue, Int.MaxValue)
    } yield LastWriterWins(time, value)
  )

  given Lattice[Int] = _ max _

  given arbORSet[A: Arbitrary]: Arbitrary[ORSet[A]] = Arbitrary(for {
    added   <- Gen.nonEmptyListOf(Arbitrary.arbitrary[A])
    removed <- Gen.listOf(Gen.oneOf(added))
  } yield {
    val a = added.foldLeft(ORSet.empty[A])((s, v) => Lattice.merge(s, s.add(v)))
    removed.foldLeft(a)((s, v) => Lattice.merge(s, s.remove(v)))
  })

  given arbMVR[A: Arbitrary]: Arbitrary[MultiValueRegister[A]] =
    val pairgen = for {
      version <- arbVersion.arbitrary
      value   <- Arbitrary.arbitrary[A]
    } yield (version, value)
    val map = Gen.listOf(pairgen).map(vs => MultiValueRegister(vs.toMap))
    Arbitrary(map)

  given arbCausalQueue[A: Arbitrary]: Arbitrary[CausalQueue[A]] =
    val pairgen = for {
      id    <- arbId.arbitrary
      value <- Arbitrary.arbitrary[A]
    } yield (id, value)
    val map = Gen.listOf(pairgen).map(_.foldLeft(CausalQueue.empty[A]) { case (acc, (id, value)) =>
      acc.enqueue(value, id)
    })
    Arbitrary(map)

  implicit val genDot: Gen[Dot] = for {
    id    <- Gen.oneOf('a' to 'g')
    value <- Gen.oneOf(0 to 100)
  } yield Dot(id.toString, value)

  implicit val arbDot: Arbitrary[Dot] = Arbitrary(genDot)

  val genDotSet: Gen[Set[Dot]] = Gen.containerOf[Set, Dot](genDot)

  val genDietMapCContext: Gen[CausalContext] = for {
    ds <- genDotSet
  } yield CausalContext.fromSet(ds)

  implicit val arbDietMapCContext: Arbitrary[CausalContext] = Arbitrary(genDietMapCContext)

  implicit val arbDotSet: Arbitrary[Set[Dot]] = Arbitrary(genDotSet)

  def genDotFun[A](implicit g: Gen[A]): Gen[Map[Dot, A]] = for {
    n      <- Gen.posNum[Int]
    dots   <- Gen.containerOfN[List, Dot](n, genDot)
    values <- Gen.containerOfN[List, A](n, g)
  } yield (dots zip values).toMap

  implicit def arbDotFun[A](implicit g: Gen[A]): Arbitrary[Map[Dot, A]] = Arbitrary(genDotFun)

  def genDotMap[K, V: WithContextDecompose](implicit gk: Gen[K], gv: Gen[V]): Gen[Map[K, V]] = (for {
    n      <- Gen.posNum[Int]
    keys   <- Gen.containerOfN[List, K](n, gk)
    values <- Gen.containerOfN[List, V](n, gv)
  } yield (keys zip values).toMap).suchThat { m =>
    val dotsIter = m.values.flatMap(v => WithContextDecompose[V].dots(v).iterator)
    val dotsSet  = dotsIter.toSet
    dotsIter.size == dotsSet.size
  }

  implicit def arbDotMap[K, V: WithContextDecompose](implicit gk: Gen[K], gv: Gen[V]): Arbitrary[Map[K, V]] =
    Arbitrary(genDotMap)
}
