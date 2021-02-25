package co.s4n.inmutable.list

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ListSpec extends AnyFlatSpec with Matchers {

  "El ejercicio 1 " should " be 9" in {
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

  "El ejercicio 2 " should " remover primer item" in {
    val lst1 = List("a","b","c","d")
    val lst2 = List("b","c","d")
    List.tail(lst1) shouldEqual lst2
  }

  "El ejercicio 3 " should " mostrar primer item" in {
    val lst = List("a","b","c","d")
    val head = "a"
    List.head(lst) shouldEqual head
  }

  "El ejercicio 4 " should "  true si todo es verdadero" in {
    val lst = List(true,true,true,true,true)
    List.and(lst) shouldEqual true
  }

  "El ejercicio 4 " should "  false si no todo es verdadero" in {
    val lst = List(true,true,false,true,true)
    List.and(lst) shouldEqual false
  }
}

