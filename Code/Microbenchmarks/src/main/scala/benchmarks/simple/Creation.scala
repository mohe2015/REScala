package benchmarks.simple

import benchmarks.EngineParam
import org.openjdk.jmh.annotations._
import rescala.interface.RescalaInterface

import java.util.concurrent.TimeUnit

@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@Warmup(iterations = 3, time = 1000, timeUnit = TimeUnit.MILLISECONDS)
@Measurement(iterations = 3, time = 1000, timeUnit = TimeUnit.MILLISECONDS)
@Fork(3)
@Threads(1)
@State(Scope.Thread)
class Creation {

  var engine: RescalaInterface = _
  final lazy val stableEngine  = engine
  import stableEngine._

  @Setup
  def setup(engineParam: EngineParam) = {
    engine = engineParam.engine
  }

  @Benchmark
  def `var`(): Var[String] = {
    Var("")
  }

  @Benchmark
  def `evt`(): Evt[String] = {
    Evt[String]()
  }

  @Benchmark
  def `derived signal`(): Signal[String] = {
    Var("").map(identity)
  }

  @Benchmark
  def `derived event`(): Event[String] = {
    Evt[String]().map(identity)
  }

}
