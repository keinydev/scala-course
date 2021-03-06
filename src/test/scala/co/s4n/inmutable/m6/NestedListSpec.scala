package co.s4n.inmutable.m6

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class NestedListSpec extends AnyFlatSpec with Matchers  {

  "flatten(Const(Elem('a') :: Elem('b') :: Const(Elem('c') :: Nil) :: Nil))" should " List('a','b','c')" in {
    NestedList.flatten(Const(Elem('a') :: Elem('b') :: Const(Elem('c') :: Nil) :: Nil)) shouldEqual List('a','b','c')
  }

  "flatten(Const(Elem(1) :: Elem(2) :: Const(Elem(3) :: Const(Elem(4) :: Elem(5) :: Nil) :: Nil) :: Nil))" should " List(1,2,3,4,5)" in {
    NestedList.flatten(Const(Elem(1) :: Elem(2) :: Const(Elem(3) :: Const(Elem(4) :: Elem(5) :: Nil) :: Nil) :: Nil)) shouldEqual List(1,2,3,4,5)
  }
}
