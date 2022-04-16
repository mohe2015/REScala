package benchmarks.lattices.delta.crdt

import kofre.decompose.DotStore.DotSet
import kofre.decompose.interfaces.EWFlagInterface.EWFlagSyntax
import kofre.syntax.AllPermissionsCtx.withID
import org.openjdk.jmh.annotations._
import rescala.extra.lattices.delta.crdt.reactive.ORMap

import java.util.concurrent.TimeUnit

@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@Warmup(iterations = 3, time = 1000, timeUnit = TimeUnit.MILLISECONDS)
@Measurement(iterations = 3, time = 1000, timeUnit = TimeUnit.MILLISECONDS)
@Fork(3)
@Threads(1)
@State(Scope.Thread)
class ORMapBench {

  @Param(Array("1", "10", "100", "1000"))
  var numEntries: Int = _

  type SUT = ORMap[Int, DotSet]

  var map: SUT = _

  @Setup
  def setup(): Unit = {
    map = (0 until numEntries).foldLeft(ORMap[Int, DotSet]("a")) {
      case (m, i) => m.mutateKey(i, (r, b) => b.enable()(withID(r)))
    }
  }

  @Benchmark
  def queryExisting(): Boolean = map.queryKey(0, _.read)

  @Benchmark
  def queryMissing(): Boolean = map.queryKey(-1, _.read)

  @Benchmark
  def containsExisting(): Boolean = map.contains(0)

  @Benchmark
  def containsMissing(): Boolean = map.contains(-1)

  @Benchmark
  def queryAllEntries(): Iterable[Boolean] = map.queryAllEntries(_.read)

  @Benchmark
  def mutateExisting(): SUT = map.mutateKey(0, (r, b) => b.disable()(withID(r)))

  @Benchmark
  def mutateMissing(): SUT = map.mutateKey(-1, (r, b) => b.enable()(withID(r)))

  @Benchmark
  def removeExisting(): SUT = map.remove(0)

  @Benchmark
  def removeMissing(): SUT = map.remove(-1)

  @Benchmark
  def removeAll(): SUT = map.removeAll(0 until numEntries)

  @Benchmark
  def removeByValue(): SUT = map.removeByValue(_.read)

  @Benchmark
  def clear(): SUT = map.clear()
}
