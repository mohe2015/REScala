package tests.distribution.delta.antientropy

import com.github.plokhotnyuk.jsoniter_scala.core._
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import kofre.decompose.containers.{AntiEntropyCRDT, Network}
import kofre.decompose.interfaces.GSetInterface.{GSet, GSetSyntax, contextDecompose}
import org.scalacheck.Prop._
import org.scalacheck.{Arbitrary, Gen}
import rescala.extra.lattices.delta.JsoniterCodecs._
import rescala.extra.replication.AntiEntropy
import tests.distribution.delta.antientropy.NetworkGenerators._

import scala.collection.mutable

object GSetGenerators {
  def genGSet[E: JsonValueCodec](implicit e: Arbitrary[E]): Gen[AntiEntropyCRDT[GSet[E]]] = for {
    elements <- Gen.containerOf[List, E](e.arbitrary)
  } yield {
    val network = new Network(0, 0, 0)
    val ae      = new AntiEntropy[GSet[E]]("a", network, mutable.Buffer())

    elements.foldLeft(AntiEntropyCRDT[GSet[E]](ae)) {
      case (set, e) => set.insert(e)
    }
  }

  implicit def arbGSet[E: JsonValueCodec](implicit e: Arbitrary[E]): Arbitrary[AntiEntropyCRDT[GSet[E]]] =
    Arbitrary(genGSet)
}

class GSetTest extends munit.ScalaCheckSuite {
  import GSetGenerators._

  implicit val intCodec: JsonValueCodec[Int] = JsonCodecMaker.make
  property("insert") {
    forAll { (set: AntiEntropyCRDT[GSet[Int]], e: Int) =>
      val setInserted = set.insert(e)

      assert(
        setInserted.elements.contains(e),
        s"The set should contain an element after it is inserted, but ${setInserted.elements} does not contain $e"
      )
    }
  }
  property("concurrent insert") {
    forAll { (e: Int, e1: Int, e2: Int) =>
      val network = new Network(0, 0, 0)

      val aea = new AntiEntropy[GSet[Int]]("a", network, mutable.Buffer("b"))
      val aeb = new AntiEntropy[GSet[Int]]("b", network, mutable.Buffer("a"))

      val sa0 = AntiEntropyCRDT[GSet[Int]](aea).insert(e)
      val sb0 = AntiEntropyCRDT[GSet[Int]](aeb).insert(e)

      AntiEntropy.sync(aea, aeb)

      val sa1 = sa0.processReceivedDeltas()
      val sb1 = sb0.processReceivedDeltas()

      assert(
        sa1.elements.contains(e),
        s"Concurrently inserting the same element should have the same effect as inserting it once, but ${sa1.elements} does not contain $e"
      )
      assert(
        sb1.elements.contains(e),
        s"Concurrently inserting the same element should have the same effect as inserting it once, but ${sb1.elements} does not contain $e"
      )

      val sa2 = sa1.insert(e1)
      val sb2 = sb1.insert(e2)

      AntiEntropy.sync(aea, aeb)

      val sa3 = sa2.processReceivedDeltas()
      val sb3 = sb2.processReceivedDeltas()

      assert(
        Set(e1, e2).subsetOf(sa3.elements),
        s"Concurrently inserting two elements should have the same effect as inserting them sequentially, but ${sa3.elements} does not contain both $e1 and $e2"
      )
      assert(
        Set(e1, e2).subsetOf(sb3.elements),
        s"Concurrently inserting two elements should have the same effect as inserting them sequentially, but ${sb3.elements} does not contain both $e1 and $e2"
      )
    }
  }
  property("convergence") {
    forAll { (insertedA: List[Int], insertedB: List[Int], network: Network) =>
      val aea = new AntiEntropy[GSet[Int]]("a", network, mutable.Buffer("b"))
      val aeb = new AntiEntropy[GSet[Int]]("b", network, mutable.Buffer("a"))

      val sa0 = insertedA.foldLeft(AntiEntropyCRDT[GSet[Int]](aea)) {
        case (set, e) => set.insert(e)
      }
      val sb0 = insertedB.foldLeft(AntiEntropyCRDT[GSet[Int]](aeb)) {
        case (set, e) => set.insert(e)
      }

      AntiEntropy.sync(aea, aeb)
      network.startReliablePhase()
      AntiEntropy.sync(aea, aeb)

      val sa1 = sa0.processReceivedDeltas()
      val sb1 = sb0.processReceivedDeltas()

      assert(
        sa1.elements == sb1.elements,
        s"After synchronization messages were reliably exchanged all replicas should converge, but ${sa1.elements} does not equal ${sb1.elements}"
      )
    }
  }
}
