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

  "El ejercicio 4 " should " devolver true si todo es verdadero" in {
    val lst = List(true,true,true,true)
    List.and(lst) shouldEqual true
  }

  "El ejercicio 4 " should " devolver false si no todo es verdadero" in {
    val lst = List(true,true,false,true)
    List.and(lst) shouldEqual false
  }

  "El ejercicio 5 " should " devolver false si todo es falso" in {
    val lst = List(false,false,false,false)
    List.or(lst) shouldEqual false
  }

  "El ejercicio 6 " should " devolver valor máximo de lista " in {
    val lst = List(3,1,8,9,2,7)
    List.max(lst) shouldEqual 9
  }

  "El ejercicio 7 " should " devolver valor mínimo de lista " in {
    val lst = List(3L,1L,8L,9L,2L,7L)
    List.min(lst) shouldEqual 1
  }

  "El ejercicio 8 " should " devolver valor mínimo y máximo de lista " in {
    val lst = List(3.0,1.0,8.0,9.0,2.0,7.0)
    List.minMax(lst) shouldEqual (1.0,9.0)
  }

  "Drop drop(2,List(1,2,3,4,5)) " should " devolver List(3,4,5) = Const(3,Const(4,Const(5,Nil)))" in {
    val lst = List(1,2,3,4,5)
    List.drop(2, lst) shouldEqual Const(3,Const(4,Const(5,Nil)))
  }

  "split split(3,List(1,2,3,4,5,6)) " should " devolver (Const(1,Const(2,Const(3,Nil))),Const(4,Const(5,Const(6,Nil)))" in {
    val lst = List(1,2,3,4,5,6)
    List.split(3, lst) shouldEqual (Const(1,Const(2,Const(3,Nil))),Const(4,Const(5,Const(6,Nil))))
  }

  "Función take: take(3,List(\"a\",\"b\",\"c\",\"d\",\"e\")) " should " devolver List(\"a\",\"b\",\"c\")" in {
    val lst = List("a","b","c","d","e")
    List.take(3, lst) shouldEqual List("a","b","c")
  }

  "Función init: init(List(1,2,3,4,5,6)) " should " devolver List(1,2,3,4,5)" in {
    val lst = List(1,2,3,4,5,6)
    List.init(lst) shouldEqual List(1,2,3,4,5)
  }

  "Función split: split(3,List(1,2,3,4,5,6,7)) " should " devolver (Const(1,Const(2,Const(3,Nil))),Const(4,Const(5,Const(6,Const(7,Nil)))))" in {
    val lst = List(1,2,3,4,5,6,7)
    List.split(3,lst) shouldEqual (Const(1,Const(2,Const(3,Nil))),Const(4,Const(5,Const(6,Const(7,Nil)))))
  }
}

