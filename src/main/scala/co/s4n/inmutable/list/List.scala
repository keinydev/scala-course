package co.s4n.inmutable.list
import scala.annotation.tailrec
sealed trait List[+A]
case object Nil extends List[Nothing]
case class Const[+A](h: A, t: List[A]) extends List[A]

object List {

   /**
    * Remueve el primer elemento de una lista
    * @param lst De cualquier tipo
    * @return lst Nueva lista
    */
   def tail[A](lst:List[A]):List[A] = lst match {
      case Nil => Nil
      case Const(h,Nil) => Nil
      case Const(h,t) => t
   }

   /**
    * Devuelve el primer elemento de una lista
    * @param lst De cualquier tipo
    * @return lst Primer item
    */
   def head[A](lst:List[A]):A = lst match {
      case Const(h,t) => h
   }

   /**
    * Devuelve true si todos los valores son verdaderos, en caso contrario devuelve false
    * @param Boolean lst
    * @return boolean
    */
   def and(lst:List[Boolean]):Boolean = lst match {
      case Nil => true
      case Const(h,t) => h && and(t)
   }

   /**
    * Devuelve false si todos los valores son falsos, en caso contrario devuelve true
    * @param Boolean lst
    * @return boolean
    */
   def or(lst:List[Boolean]):Boolean = lst match {
      case Const(true,Nil) => true
      case Const(false,Nil) => false
      case Const(true,t) => true
      case Const(false,t) => or(t)
   }

   /**
    * Esta función recibe un arreglo de valores Int y devuelve valor máximo de todos los valores en la lista
    * @param Int lst
    * @return Int Retorna la lista
    */
   def max(lst:List[Int]):Int = {
      @tailrec
      def maxInterno(lst:List[Int], valorMax:Int):Int = lst match {
         case Nil => valorMax
         case Const(h,t) => maxInterno(t, if(h > valorMax) h else valorMax)
      }
      maxInterno(lst, 1)
   }

   /**
    * Esta función recibe un arreglo de valores Long y devuelve valor mı́nimo de todos los valores en la lista.
    * @param Long lst
    * @return Long Retorna la lista
    */
   def min(lst:List[Long]): Long = {
      @tailrec
      def minInterno(lst:List[Long], valorMin:Long):Long = lst match {
         case Nil => valorMin
         case Const(h,t) => minInterno(t, if(h < valorMin) h else valorMin)
      }
      minInterno(lst, 1)
   }

   /**
    * Esta función recibe un arreglo de valores Double y devuelve valor (mı́nimo,máximo) de todos los valores en la lista
    * @param Double lst
    * @return Long Retorna las tuplas del resultas
    */
   def minMax(lst:List[Double]):(Double, Double) = {

      @tailrec
      def minInterno(lst:List[Double], valorMin:Double):Double = lst match {
         case Nil => valorMin
         case Const(h, t) => minInterno(t, if(h < valorMin) h else valorMin)
      }

      def maxInterno(lst:List[Double], valorMax:Double):Double = lst match {
         case Nil => valorMax
         case Const(h, t) => maxInterno(t, if(h > valorMax) h else valorMax)
      }
      val maximo:Double = maxInterno(lst,0)
      val minimo:Double = minInterno(lst, List.head(lst))

      (minimo, maximo)
   }

   /**
    * Devuelve la longitud de la lista
    * @param Int lst
    * @return boolean
    */
   def length[A](lst:List[A]):Int = lst match {
      case Nil => 0
      case Const(h,t) => 1 + length(t)
   }

   /**
    * Devuelve la suma de la lista
    * @param Int lst
    * @return boolean
    */
   def sum(ints: List[Int]):Int = ints match {
      case Nil => 0
      case Const(h,t) => h + sum(t)
   }

   /**
    * Devuelve el producto de la lista
    * @param Double lst
    * @return boolean
    */
   def product(ds: List[Double]):Double = ds match{
      case Nil => 1
      case Const(h,t) => h * product(t)
   }
   
   def apply[A](as: A*) : List[A] = {
      if (as.isEmpty) Nil
      else Const(as.head, apply (as.tail: _*))
   }
}

