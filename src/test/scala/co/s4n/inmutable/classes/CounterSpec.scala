package co.s4n.inmutable.classes

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class CounterSpec extends AnyFlatSpec with Matchers {

  "new Counter(10).incr.decr.incr.incr.contador " should " 12 " in {
    new Counter(10).incr.decr.incr.incr.contador shouldEqual 12
  }

  "new Counter(1).decr.contador " should " 0 " in {
    new Counter(1).decr.contador shouldEqual 0
  }

  "new Counter(5).incr.incr.incr.contador " should " 8 " in {
    new Counter(5).incr.incr.incr.contador shouldEqual 8
  }

  "new Counter(5).incr(10).incr.incr.contador " should " 17 " in {
    new Counter(5).incr(10).incr.incr.contador shouldEqual 17
  }

  "new Counter(5).incr.decr(2).contador " should " 4 " in {
    new Counter(5).incr.decr(2).contador shouldEqual 4
  }

  "Usando sumador " should " incrementando el monto original " in {
    val sum = new Sumador(10)
    val cont = new Counter(2)
    cont.ajuste(sum).contador shouldEqual 12
  }
}