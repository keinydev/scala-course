package co.s4n.inmutable.m6

sealed trait NestedList[+A]
case class Elem[A](elem:A) extends NestedList[A]
case class Const[A](lst: List[NestedList[A]]) extends NestedList[A]

object NestedList {
  /**
   * Transforma una lista, posiblemente manteniendo listas como elementos en una lista "plana",
   * reemplazando cada lista con sus elementos (recursivamente).
   * @param lst Lista
   * @return Lista plana
   */
  def flatten[A](lst: NestedList[A]):List[A] = lst match {
    case Elem(x) => List(x)
    case Const(h :: t) => flatten(h) ::: flatten(Const(t))
    case Const(Nil) => Nil
  }
  //NestedList.flatten(Const(Elem(1) :: Elem(2) :: Const(Elem(3) :: Nil) :: Nil))
  //NestedList.flatten(Const(Elem(1) :: Elem(2) :: Const(Elem(3) :: Const(Elem(4) :: Elem(5) :: Nil) :: Nil) :: Nil))
}
