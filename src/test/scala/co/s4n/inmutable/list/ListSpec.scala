package co.s4n.inmutable.list

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ListSpec extends AnyFlatSpec with Matchers {
  "El ejercicio 1 " should " 9" in {
    val x = List(4,5,6,7,8) match{
      case Const(x, Const(5, Const(7,_))) =>x
      case Nil => 1
      case Const(x, Const(y, Const(6,Const(7,_)))) => x + y
      case Const(h,t) => h+ List.sum(t)
      case _ => 777
    }
    val y = List
    x shouldEqual + 9
  }
}

