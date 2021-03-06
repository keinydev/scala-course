package co.s4n.inmutable.m6

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class NinetyNineQuestionsSpec extends AnyFlatSpec with Matchers  {

  "last(List(1,2,3))" should " 3" in {
    NinetyNineQuestions.last(List(1,2,3)) shouldEqual 3
  }

  "penultimate(List(1,2,3))" should " 2" in {
    NinetyNineQuestions.penultimate(List(1,2,3)) shouldEqual 2
  }

  "twoLastElements(List(1,2,3))" should " List(2,3)" in {
    NinetyNineQuestions.twoLastElements(List(1,2,3)) shouldEqual List(2,3)
  }

  "elementPosition(3,List(\"a\",\"b\",\"c\",\"d\",\"e\"))" should " c" in {
    NinetyNineQuestions.elementPosition(3,List("a","b","c","d","e")) shouldEqual "c"
  }

  "length(List(1,2,3))" should " 3" in {
    NinetyNineQuestions.length(List(1,2,3)) shouldEqual 3
  }

  "reverse(List(1,2,3))" should " List(3,2,1)" in {
    NinetyNineQuestions.reverse(List(1,2,3)) shouldEqual List(3,2,1)
  }

//  "isPalindrome(List(1,2,4,8,16,8,4,2,1))" should " true" in {
//    NinetyNineQuestions.isPalindrome(List(1,2,4,8,16,8,4,2,1)) shouldEqual true
//  }

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

//  "replicate(List(1,2,3))" should " List(1, 1, 1, 2, 2, 2, 3, 3, 3)" in {
//    NinetyNineQuestions.replicate(List(1,2,3)) shouldEqual List(1, 1, 1, 2, 2, 2, 3, 3, 3)
//  }

  "split(3,List(1,2,3,4,5,6,7,8,9,10))" should " (List(1, 2, 3),List(4, 5, 6, 7, 8, 9, 10))" in {
    NinetyNineQuestions.split(3,List(1,2,3,4,5,6,7,8,9,10)) shouldEqual (List(1, 2, 3),List(4, 5, 6, 7, 8, 9, 10))
  }
}
