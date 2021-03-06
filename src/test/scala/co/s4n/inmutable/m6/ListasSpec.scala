package co.s4n.inmutable.m6

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ListasSpec extends AnyFlatSpec with Matchers  {

  "subs(List(1,2)).length " should " List(List(),List(1),List(2),List(1,2)).length" in {
    Listas.subs(List(1,2)).length shouldEqual List(List(),List(1),List(2),List(1,2)).length
  }

  "permutaciones(List(\"a\",\"b\",\"c\")) " should " List(List(\"a\",\"b\",\"c\"),List(\"b\",\"a\",\"c\"),List(\"b\",\"c\",\"a\"),List(\"a\",\"c\",\"b\"),List(\"c\",\"a\",\"b\"),List(\"c\",\"b\",\"a\"))" in {
    Listas.permutaciones(List("a","b","c")) shouldEqual List(List("a","b","c"),List("b","a","c"),List("b","c","a"),List("a","c","b"),List("c","a","b"),List("c","b","a"))
  }

  "permutaciones(List(2,3,4)) " should " List(List(2,3,4),List(3,2,4),List(3,4,2),List(2,4,3),List(4,2,3),List(4,3,2))" in {
    Listas.permutaciones(List(2,3,4)) shouldEqual List(List(2,3,4),List(3,2,4),List(3,4,2),List(2,4,3),List(4,2,3),List(4,3,2))
  }
}
