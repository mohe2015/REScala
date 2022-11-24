package tests.rescala

import tests.rescala.testtools.RETests


class Fold3Test extends RETests {
  multiEngined { engine =>
    import engine._

    test("scala 3 fold expressions") {

      val word  = Evt[String]()
      val count = Evt[Int]()
      val reset = Evt[Unit]()

      val resetB = reset act (_ => "")

      val wordB  = word act identity
      val countB = count act (current * _)

      val res = Fold("")(resetB, wordB, countB)

      assert(res.readValueOnce == "")
      count.fire(10)
      assert(res.readValueOnce == "")
      reset.fire()
      assert(res.readValueOnce == "")
      word.fire("hello")
      assert(res.readValueOnce == "hello")
      count.fire(2)
      assert(res.readValueOnce == "hellohello")
      word.fire("world")
      assert(res.readValueOnce == "world")
      transaction(count, word, reset) { implicit at =>
        count.admit(2)
        word.admit("do them all!")
        reset.admit(())

      }
      assert(res.readValueOnce == "do them all!do them all!")
    }

  }
}
