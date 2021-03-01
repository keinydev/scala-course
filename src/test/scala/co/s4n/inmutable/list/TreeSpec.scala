package co.s4n.inmutable.list

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class TreeSpec extends AnyFlatSpec with Matchers {

  "Tamaño de Leaf(10) " should " 1" in {
    Tree.size(Leaf(10)) shouldEqual 1
  }

  "Tamaño de Branch(Leaf(10),Leaf(20)) " should " 3" in {
    Tree.size(Branch(Leaf(10),Leaf(20))) shouldEqual 3
  }

  "Tamaño de Branch(Branch(Leaf(10),Leaf(20)),Leaf(30)) " should " 5" in {
    Tree.size(Branch(Branch(Leaf(10),Leaf(20)),Leaf(30))) shouldEqual 5
  }

  "Depth de Leaf(10) " should " 1" in {
    Tree.depth(Leaf(10)) shouldEqual 1
  }

  "Depth de Branch(Leaf(10),Leaf(20)) " should " 2" in {
    Tree.depth(Branch(Leaf(10),Leaf(20))) shouldEqual 2
  }

  "Depth de Branch(Branch(Leaf(10),Leaf(20)),Leaf(30)) " should " 3" in {
    Tree.depth(Branch(Branch(Leaf(10),Leaf(20)),Leaf(30))) shouldEqual 3
  }

  "Depth de Branch(Branch(Leaf(10),Leaf(20)),Branch(Leaf(30),Leaf(40))) " should " 3" in {
    Tree.depth(Branch(Branch(Leaf(10),Leaf(20)),Branch(Leaf(30),Leaf(40)))) shouldEqual 3
  }
}