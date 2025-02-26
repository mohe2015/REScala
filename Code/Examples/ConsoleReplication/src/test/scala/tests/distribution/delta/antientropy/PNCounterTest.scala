package tests.distribution.delta.antientropy

import kofre.datatypes.PosNegCounter
import org.scalacheck.{Arbitrary, Gen}
import rescala.extra.lattices.delta.JsoniterCodecs._

import rescala.extra.replication.AntiEntropy
import kofre.decompose.containers.{AntiEntropyCRDT, Network}
import NetworkGenerators._
import org.scalacheck.Prop._

import scala.collection.mutable

object PosNegCounterGenerator {
  val genPosNegCounter: Gen[AntiEntropyCRDT[PosNegCounter]] = for {
    nInc <- Gen.posNum[Int]
    nDec <- Gen.posNum[Int]
  } yield {
    val network = new Network(0, 0, 0)
    val ae      = new AntiEntropy[PosNegCounter]("a", network, mutable.Buffer())

    val inced = (0 to nInc).foldLeft(AntiEntropyCRDT(ae)) {
      case (c, _) => c.inc()
    }

    (0 to nDec).foldLeft(inced) {
      case (c, _) => c.dec()
    }
  }

  implicit val arbPosNegCounter: Arbitrary[AntiEntropyCRDT[PosNegCounter]] = Arbitrary(genPosNegCounter)
}

class PosNegCounterTest extends munit.ScalaCheckSuite {
  import PosNegCounterGenerator._
  property("inc") {
    forAll { (counter: AntiEntropyCRDT[PosNegCounter]) =>
      val before = counter.value
      val inced  = counter.inc()

      assert(
        inced.value == before + 1,
        s"Incrementing the counter should increase its value by 1, but ${inced.value} does not equal ${counter.value} + 1"
      )
    }
  }
  property("dec") {
    forAll { (counter: AntiEntropyCRDT[PosNegCounter]) =>
      val before = counter.value
      val deced  = counter.dec()

      assert(
        deced.value == before - 1,
        s"Decrementing the counter should decrease its value by 1, but ${deced.value} does not equal ${counter.value} - 1"
      )
    }
  }
  property("concurrent") {
    forAll { (incOrDecA: Boolean, incOrDecB: Boolean) =>
      val network = new Network(0, 0, 0)

      val aea = new AntiEntropy[PosNegCounter]("a", network, mutable.Buffer("b"))
      val aeb = new AntiEntropy[PosNegCounter]("b", network, mutable.Buffer("a"))
      val aec = new AntiEntropy[PosNegCounter]("c", network, mutable.Buffer("c"))

      val ca0 = if (incOrDecA) AntiEntropyCRDT[PosNegCounter](aea).inc() else AntiEntropyCRDT[PosNegCounter](aea).dec()
      val cb0 = if (incOrDecB) AntiEntropyCRDT[PosNegCounter](aeb).inc() else AntiEntropyCRDT[PosNegCounter](aeb).dec()

      AntiEntropy.sync(aea, aeb)

      val ca1 = ca0.processReceivedDeltas()
      val cb1 = cb0.processReceivedDeltas()

      val sequential = AntiEntropyCRDT(aec)
      if (incOrDecA) sequential.inc() else sequential.dec()
      if (incOrDecB) sequential.inc() else sequential.dec()

      assert(
        ca1.value == sequential.value,
        s"Concurrent execution of increment or decrement should be equivalent to any sequential execution, but ${ca1.value} does not equal ${sequential.value}"
      )

      assert(
        cb1.value == sequential.value,
        s"Concurrent execution of increment or decrement should be equivalent to any sequential execution, but ${cb1.value} does not equal ${sequential.value}"
      )
    }
  }
  property("convergence") {
    forAll { (incA: Short, decA: Short, incB: Short, decB: Short, network: Network) =>
      val aea = new AntiEntropy[PosNegCounter]("a", network, mutable.Buffer("b"))
      val aeb = new AntiEntropy[PosNegCounter]("b", network, mutable.Buffer("a"))

      val incedA = (0 until incA.toInt).foldLeft(AntiEntropyCRDT[PosNegCounter](aea)) {
        case (c, _) => c.inc()
      }
      val ca0 = (0 until decA.toInt).foldLeft(incedA) {
        case (c, _) => c.dec()
      }
      val incedB = (0 until incB.toInt).foldLeft(AntiEntropyCRDT[PosNegCounter](aeb)) {
        case (c, _) => c.inc()
      }
      val cb0 = (0 until decB.toInt).foldLeft(incedB) {
        case (c, _) => c.dec()
      }

      AntiEntropy.sync(aea, aeb)
      network.startReliablePhase()
      AntiEntropy.sync(aea, aeb)

      val ca1 = ca0.processReceivedDeltas()
      val cb1 = cb0.processReceivedDeltas()

      assert(
        ca1.value == cb1.value,
        s"After synchronization messages were reliably exchanged all replicas should converge, but ${ca1.value} does not equal ${cb1.value}"
      )
    }
  }
}
