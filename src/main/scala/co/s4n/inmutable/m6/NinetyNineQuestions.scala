package co.s4n.inmutable.m6
import scala.annotation.tailrec

object NinetyNineQuestions {

  /**
   * Devuelve el último elemento de una lista
   * Precondición: La lista no debe ser vacía
   * @param ls Lista
   * @return El último elemento de la lista
   */
  def last[A](lst: List[A]):A = lst match {
    case head :: Nil  => head
    case _ :: tail => last(tail)
  }

  /**
   * Devuelve el penúltimo elemento de la lista
   * Precondición: La lista no debe ser vacía
   * @param ls Lista
   * @return Penúltimo elemento
   */
  def penultimate[A](lst: List[A]):A = lst match {
    case head :: _ :: Nil => head
    case _ :: tail     => penultimate(tail)
  }

  /**
   * Devuelve los dos ultimos elementos de una lista
   * Precondición: La lista debe tener dos o más elementos
   * @param lst Lista
   * @return Lista con dos últimos valores
   */
  def twoLastElements[A](lst: List[A]):List[A] = lst match {
    case x :: y :: Nil => List(x,y)
    case _ :: y => twoLastElements(y)
  }

  /***
   * Devuelve el valor que hay la posición buscada
   * Precondición: La lista no debe ser vacía
   * @param n   Posición
   * @param lst Lista
   * @return El valor en la posición
   */
  def elementPosition[A](n: Int, lst: List[A]):A = (n, lst) match {
    case (1, head :: _  ) => head
    case (n, _ :: tail) => elementPosition(n - 1, tail)
  }

  /**
   * Devuelve longitud de la lista
   * @param lst Lista
   * @return Longitud
   */
  def length[A](lst: List[A]):Int = lst match {
    case Nil       => 0
    case _ :: tail => 1 + length(tail)
  }

  /**
   * Devuelve longitud de la lista usando foldRight
   * @param lst Lista
   * @return Longitud
   */
  def lengthFoldRight[A](lst: List[A]):Int = lst.foldRight(0)((_,y) => 1 + y)

  /**
   * Devuelve longitud de la lista usando foldLeft
   * @param lst Lista
   * @return Longitud
   */
  def lengthFoldLeft[A](lst: List[A]):Int =  lst.foldLeft(0)((x,_) => x + 1 )

  /**
   * Devuelve una lista revertida
   * @param lst Lista
   * @return Lista en reverso
   */
  def reverse[A](lst: List[A]): List[A] = lst match {
    case Nil => Nil
    case head :: tail => reverse(tail) ::: List(head)
  }

  /**
   * Devuelve una lista revertida usando recursión
   * @param lst Lista
   * @return Lista en reverso
   */
  def reverseRecursive[A](lst: List[A]): List[A] = {
    @tailrec
    def reverseInterno(lst: List[A], acum: List[A]): List[A] = lst match {
      case Nil       => acum
      case head :: tail => reverseInterno(tail, head :: acum)
    }

    reverseInterno(lst, Nil)
  }

  /**
   * Devuelve true si la lista es un palindrome
   * @param lst Lista
   * @return Devuelve validación
   */
  def isPalindrome[A](lst: List[A]):Boolean = ???

  /**
   * Elimina los elementos duplicados consecutivos en una lista
   * @param lst Lista
   * @return Lista sin duplicados
   */
  def compress[A](lst: List[A]):List[A] = lst match {
    case Nil => Nil
    case x :: y :: tail if x == y => compress(y :: tail)
    case x :: tail => x :: compress(tail)
  }

  /**
   * Empaqueta duplicados consecutivos de una lista en sublistas. Si una lista contiene elementos repetidos, deben colocarse en sublistas separadas.
   * @param list Lista
   * @return Lista de listas por elementos duplicados
   */

  def pack[A](list: List[A]): List[List[A]] = {
    def packInterno[A](list: List[A], acum: List[A]): List[List[A]] = list match {
      case head :: tail if acum.isEmpty => packInterno(tail, List(head))
      case head :: tail => if (head == acum.head) packInterno(tail, head :: acum) else acum :: packInterno(tail, List(head))
      case Nil => List(acum)
    }

    packInterno(list, List())
  }

  /**
   * Computa la cantidad de duplicados de las sublistas del problema anterior usando la compresión de datos.
   * Los duplicados consecutivos se muestran como listas (N,E) donde N es el número de duplicados del elemento E.
   * @param lst Lista
   * @return Lista con tuplas tipo (N,E)
   */
  def encode[A](lst: List[A]):List[(Int, A)] = {
    @tailrec
    def encodeInterno(lst: List[List[A]], acum: List[(Int, A)]):List[(Int, A)] = lst match {
      case Nil => acum
      case head :: tail => encodeInterno(tail, acum:::List((head.length, head.head)))
    }
    encodeInterno(pack(lst), List())
  }

  /**
   * Duplicar cada elemento de la lista
   * @param lst Lista
   * @return Lista duplicada
   */
  def duplicate[A](lst: List[A]): List[A] = lst match {
    case Nil => Nil
    case head :: Nil => head :: head :: Nil
    case head :: tail => head :: head :: duplicate(tail)
  }

//  def replicate(lst: List[A]):List[A] = {

  /**
   * Elimina cada n posición que indique el usuario.
   * @param lst Lista
   * @param n Cada n posición
   * @return Lista sin posiciones indicadas
   */
  def drop[A](lst: List[A], n: Int): List[A] = {
    @tailrec
    def dropInterno(i: Int, lst: List[A], acum: List[A]): List[A] = (i, lst) match {
      case (_, Nil) => acum
      case (1, head :: tail) => dropInterno(n, tail, acum)
      case (i, head :: tail) => dropInterno(i - 1, tail, acum ::: List(head))
    }
    dropInterno(n, lst, Nil)
  }

  /**
   * Permite hacer split a una lista, por ejemplo split(3,List(1,2,3,4,5,6)) = (List(1,2,3),List(4,5,6))
   * @param n Cantidad a recortar
   * @param lst Lista
   * @return retorna una lista doble
   */
  def split[A](n:Int,lst:List[A]):(List[A],List[A]) = {

    @tailrec
    def splitInterno[A](n:Int, lst:List[A], acum:List[A]):(List[A],List[A]) = (n,lst) match {
      case (_,Nil) => (acum,Nil)
      case (0,lst) => (acum,lst)
      case (n,head :: tail) => splitInterno(n-1, tail, acum ::: List(head))
    }
    splitInterno(n, lst, List())
  }

  /**
   * Devuelve una Lista entre el rango de la posición i y k.
   * Se valida que el indice se encuentre entre el inicio y el ultimo indice requerido para devolver el head, de lo contrario se llena con el acum actual
   * @param lst Lista
   * @param inicio Inicio del slice
   * @param ultimo Última posición del slice
   * @return Lista con subset de datos
   */
  def slice[A](lst: List[A], inicio: Int, ultimo: Int):List[A] = {
    @tailrec
    def sliceInterno(index: Int, lst: List[A], acum: List[A]):List[A] = lst match {
      case head :: tail if index >= inicio - 1 && index < ultimo => sliceInterno(index + 1, tail, acum ::: List(head))
      case head :: tail => sliceInterno(index + 1, tail, acum)
      case _ => acum
    }
    sliceInterno(0, lst, List())
  }



}