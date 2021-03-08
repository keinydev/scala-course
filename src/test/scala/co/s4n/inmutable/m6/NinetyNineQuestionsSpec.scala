package co.s4n.inmutable.m6

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class NinetyNineQuestionsSpec extends AnyFlatSpec with Matchers  {

  "headFor(List(List(1,2),List(3,4)))" should " List(1,2)" in {
    NinetyNineQuestions.headFor(List(List(1,2),List(3,4))) shouldEqual List(1,2)
  }

  "last(List(1,2,3))" should " 3" in {
    NinetyNineQuestions.last(List(1,2,3)) shouldEqual 3
  }

  "lastFor(List(1,2,3,4,5,6))" should " 6" in {
    NinetyNineQuestions.lastFor(List(1,2,3,4,5,6)) shouldEqual 6
  }

  "penultimate(List(1,2,3))" should " 2" in {
    NinetyNineQuestions.penultimate(List(1,2,3)) shouldEqual 2
  }

  "twoLastElements(List(1,2,3))" should " List(2,3)" in {
    NinetyNineQuestions.twoLastElements(List(1,2,3)) shouldEqual List(2,3)
  }

  "elementPosition(3,List(\"a\",\"b\",\"c\",\"d\",\"e\"))" should " c" in {
    NinetyNineQuestions.elementAt(3,List("a","b","c","d","e")) shouldEqual "c"
  }

  "elementAtFor(2,List(1,2,3))" should " 3" in {
    NinetyNineQuestions.elementAtFor(2,List(1,2,3)) shouldEqual 3
  }

  "elementAtFor(4,List(1,2,3))" should " -1" in {
    NinetyNineQuestions.elementAtFor(4,List(1,2,3)) shouldEqual -1
  }

  "length(List(1,2,3))" should " 3" in {
    NinetyNineQuestions.length(List(1,2,3)) shouldEqual 3
  }

  "lengthFor(List())" should " 0" in {
    NinetyNineQuestions.lengthFor(List()) shouldEqual 0
  }

  "reverse(List(1,2,3))" should " List(3,2,1)" in {
    NinetyNineQuestions.reverse(List(1,2,3)) shouldEqual List(3,2,1)
  }

  "isPalindrome(List(1,2,4,8,16,8,4,2,1))" should " true" in {
    NinetyNineQuestions.isPalindrome(List(1,2,4,8,16,8,4,2,1)) shouldEqual true
  }

  "compress(List(1,2,2,3,4,4,5,6,6,6,7,8,8,9))" should " List(1,2,3,4,5,6,7,8,9)" in {
    NinetyNineQuestions.compress(List(1,2,2,3,4,4,5,6,6,6,7,8,8,9)) shouldEqual List(1,2,3,4,5,6,7,8,9)
  }

  "pack(List('a', 'a', 'a', 'a', 'b', 'c', 'c', 'd', 'e', 'e', 'e', 'e'))" should " List(List('a', 'a', 'a', 'a'), List('b'), List('c', 'c'), List('d'), List('e', 'e', 'e', 'e'))" in {
    NinetyNineQuestions.pack(List('a', 'a', 'a', 'a', 'b', 'c', 'c', 'd', 'e', 'e', 'e', 'e')) shouldEqual List(List('a', 'a', 'a', 'a'), List('b'), List('c', 'c'), List('d'), List('e', 'e', 'e', 'e'))
  }

  "encode(List('a', 'a', 'a', 'a', 'b', 'c', 'c', 'd', 'e', 'e', 'e', 'e'))" should " List((4,'a'), (1,'b'), (2,'c'), (1,'d'), (4,'e'))" in {
    NinetyNineQuestions.encode(List('a', 'a', 'a', 'a', 'b', 'c', 'c', 'd', 'e', 'e', 'e', 'e')) shouldEqual List((4,'a'), (1,'b'), (2,'c'), (1,'d'), (4,'e'))
  }

  "duplicate(List(1,2,3))" should " List((4,'a'), (1,'b'), (2,'c'), (1,'d'), (4,'e'))" in {
    NinetyNineQuestions.duplicate(List(1,2,3)) shouldEqual List(1, 1, 2, 2, 3, 3)
  }

  "replicate(List(1,2,3))" should " List(1, 1, 1, 2, 2, 2, 3, 3, 3)" in {
    NinetyNineQuestions.replicate(List(1,2,3),3) shouldEqual List(1, 1, 1, 2, 2, 2, 3, 3, 3)
  }

  "split(3,List(1,2,3,4,5,6,7,8,9,10))" should " (List(1, 2, 3),List(4, 5, 6, 7, 8, 9, 10))" in {
    NinetyNineQuestions.split(3,List(1,2,3,4,5,6,7,8,9,10)) shouldEqual (List(1, 2, 3),List(4, 5, 6, 7, 8, 9, 10))
  }

  "slice(List(1,2,3,4,5,6,7,8,9,10),3,7)" should " List(3, 4, 5, 6, 7)" in {
    NinetyNineQuestions.slice(List(1,2,3,4,5,6,7,8,9,10),3,7) shouldEqual List(3, 4, 5, 6, 7)
  }

  "rotate(3, List('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k')) " should " List('d', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'a', 'b', 'c')" in {
    NinetyNineQuestions.rotate(3, List('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k')) shouldEqual List('d', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'a', 'b', 'c')
  }

  "removeAt(2,List('a','b','x')) " should " (List('a', 'b'),'c')" in {
    NinetyNineQuestions.removeAt(2,List('a','b','c')) shouldEqual (List('a', 'b'),'c')
  }

  "insertAt('w',List('a','b','c'),1) " should " List('w','a','b','c')" in {
    NinetyNineQuestions.insertAt('w',List('a','b','c'),1) shouldEqual List('w','a','b','c')
  }

  "range(1,5) " should " List(1, 2, 3, 4, 5)" in {
    NinetyNineQuestions.range(1,5) shouldEqual List(1, 2, 3, 4, 5)
  }

  "range(5,-1) " should " List(5, 4, 3, 2, 1, 0, -1)" in {
    NinetyNineQuestions.range(5,-1) shouldEqual List(5, 4, 3, 2, 1, 0, -1)
  }

  "combinations(2,List('a','b','c','d')) " should " List(List('a', 'b'), List('a', 'c'), List('a', 'd'), List('b', 'c'), List('b', 'd'), List('c', 'd'))" in {
    NinetyNineQuestions.combinations(2,List('a','b','c','d')) shouldEqual List(List('a', 'b'), List('a', 'c'), List('a', 'd'), List('b', 'c'), List('b', 'd'), List('c', 'd'))
  }
}
