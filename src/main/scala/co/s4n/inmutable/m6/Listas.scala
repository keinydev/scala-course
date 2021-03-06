package co.s4n.inmutable.m6

object Listas {

  /**
   * Permite obtener todos los subconjuntos que un conjunto tiene
   * @param lst
   * @return Lista nueva
   */
  def subs[A](lst:List[A]):List[List[A]] = lst match {
    case Nil => List(Nil)
    case head :: tail => subs(tail) ::: subs(tail).map(head :: _)
  }


  def barajar[A](a:A, lst:List[A]):List[List[A]] = lst match {
    case Nil => List(List(a))
    case x :: xs => (a :: (x :: xs)) :: (barajar(a, xs)).map(x::_)
  }

  def permutaciones[A](lst:List[A]):List[List[A]] = lst match {
    case Nil => List(Nil)
    case x :: xs => (permutaciones(xs)).flatMap(barajar(x,_))
  }

 // def predAtPos[A](lst:List[A], preds:List[(Int, p:A => Boolean)]):List[[Boolean]] = ???

}
