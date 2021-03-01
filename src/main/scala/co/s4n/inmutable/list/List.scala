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
    * @param lst Lista
    * @return boolean
    */
   def length[A](lst:List[A]):Int = lst match {
      case Nil => 0
      case Const(h,t) => 1 + length(t)
   }

   /**
    * Devuelve la suma de la lista
    * @param lst Lista
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

   /**
    * Permite añadir un nuevo elemento al inicio de la lista
    * @param h head
    * @param t Lista
    * @return Nueva lista
    */
   def const[A](h:A, t:List[A]):List[A] = Const(h,t)

   /**
    * Permite añadir un nuevo elemento al final de la lista
    * @param h  Elemento a adicionar
    * @param t  Lista
    * @return Nueva lista
    */
   def addEnd[A](lst:List[A],elem:A):List[A] = lst match {
      case Nil => Const(elem,Nil)
      case Const(h, t) => Const(h, addEnd(t,elem))
   }

   /**
    * Se encarga de agregar una lista 2 a una lista 1
    * @param lst1 Lista 1
    * @param lst2 Lista 2
    * @return una lista unificada
    */
   def append[A](lst1:List[A], lst2:List[A]):List[A] = (lst1,lst2) match {
      case (Nil,Nil) => Nil
      case (lst1,Nil) => lst1
      case (Nil,lst2) => lst2
      case (Const(h,t),lst2) => Const(h, append(t, lst2))
   }

   /**
    * Eliminar n posiciones de la lista
    * @param Int n Posición
    * @param lst Lista
    * @return Retorna la lista sin las primeras n posiciones
    */
   def drop[A](n:Int,lst:List[A]):List[A] = (n,lst) match{
      case (0,lst) => lst
      case (n,Nil) => Nil
      case (n,Const(h,t)) => drop(n-1,t)
   }

   /**
    * Esta función elimina de la lista elementos según la condición de la función recibida
    * @param lst Lista
    * @param f Función
    * @return Nuevos valores computados
    */
   def dropWhile[A](lst:List[A])(f:A=>Boolean):List[A] = lst match{
      case Nil => Nil
      case Const(h,t) if f(h) => dropWhile(t)(f)
      case _ => lst
   }

   /**
    * Esta función se encarga de tomar los n primeros valores, si existen de la lista y retornarlos
    * @param Int n
    * @param lst Lista
    * @return La lista con los primeros n valores
    */
   def take[A](n:Int, lst:List[A]):List[A] = {
      /**
       * Esta función interna se encarga de ir acumulando los valores en una nueva lista
       * @param Int n
       * @param lst  Lista original
       * @param acum Lista auxiliar
       * @return La lista con los primeros n valores
       */
      @tailrec
      def takeInterno(n:Int, lst:List[A], acum:List[A]):List[A] = (n,lst) match {
         case (0,lst) => acum
         case (_,Nil) => acum
         case (n,Const(h,t)) => takeInterno(n-1, t, append(acum,List(h)))
      }
      takeInterno(n, lst, Nil)
   }

   /**
    * Esta función toma una lista y toma los valores iniciales excepto el último
    * @param lst Lista
    * @return Retorna la lista sin el último valor
    */
   def init[A](lst:List[A]):List[A] = lst match {
      case Nil => sys.error("La lista no puede ser vacía")
      case Const(_,Nil) => Nil
      case Const(h,t) => Const(h,init(t))
   }

   /**
    * Permite hacer split a una lista, por ejemplo split(3,List(1,2,3,4,5,6)) = (List(1,2,3),List(4,5,6))
    * @param Int n Cantidad a recortar
    * @param lst Lista
    * @return retorna una lista doble
    */
   def split[A](n:Int,lst:List[A]):(List[A],List[A]) = {
      /**
       * Esta función agrega en una lista vacía el acumulador, si "n" es cero entonces devuelve el acumulador + el tail de la lista
       * @param Int n
       * @param lst Lista original
       * @param acum Lista auxiliar
       * @return
       */
      @tailrec
      def splitInterno[A](n:Int, lst:List[A], acum:List[A]):(List[A],List[A]) = (n,lst) match {
         case (_,Nil) => (acum,Nil)
         case (0,lst) => (acum,lst)
         case (n,Const(h,t)) => splitInterno(n-1, t, append(acum,List(h)))
      }
      splitInterno(n, lst, List())
   }

   /**
    * Fusiona dos listas de tipos diferentes en una lista de pares del mismo tamaño
    * @param lst1 Lista 1
    * @param lst2 Lista 2
    * @return Lista fusionada en pares
    */
   def zip[A,B](lst1:List[A], lst2:List[B]):List[(A,B)] = {
      /**
       * Esta función arma los pares en la nueva lista
       * @param lst1 Lista 1 original
       * @param lst2 Lista 2 original
       * @param acum Lista auxiliar
       * @return Lista nueva fusionada
       */
      @tailrec
      def zipInterno(lst1:List[A], lst2:List[B], acum:List[(A,B)]):List[(A,B)] = (lst1,lst2) match {
         case (Nil,_) => acum
         case (_,Nil) => acum
         case (Const(h1,t1),Const(h2,t2)) => zipInterno(t1,t2,addEnd(acum,(h1,h2)))
      }
      zipInterno(lst1,lst2,Nil)
   }

   /**
    * Separa una lista de tuplas en dos listas distinta
    * @param lst Lista con combinación de tuplas
    * @return Lista con dos listas independientes
    */
   def unzip[A,B](lst:List[(A,B)]):(List[A],List[B]) = {
      /**
       * Separa las tuplas en lista por separado
       * @param lst Lista original con combinación de tuplas
       * @param acum1 Lista auxiliar 1
       * @param acum2 Lista auxiliar 2
       * @return Lista con dos listas independientes
       */
      @tailrec
      def unzipInterno(lst:List[(A,B)],acum1:List[A],acum2:List[B]):(List[A],List[B]) = lst match {
         case Nil => (acum1,acum2)
         case Const((h1,h2),t) => unzipInterno(t,addEnd(acum1,h1),addEnd(acum2,h2))
      }
      unzipInterno(lst,Nil,Nil)
   }

   /**
    * Toma una lista y devuelve una versión invertida de la misma
    * @param lst Lista
    * @return Lista invertida
    */
   def reverse[A](lst:List[A]):List[A] = {
      /**
       * Esta función invierte el orden de la lista, pasando primero el tail y luego el acumulador
       * @param lst Lista original
       * @param acum Lista auxiliar
       * @return
       */
      @tailrec
      def reverseInterno(lst: List[A], acum:List[A]): List[A] = lst match {
         case Nil => acum
         case Const(h,t) => reverseInterno(t,Const(h,acum))
      }
      reverseInterno(lst, Nil)
   }

   /**
    * Esta función se encarga de entremezclar un valor entre los elementos originales de la lista
    * @param elem Valor a mezclar
    * @param lst Lista
    * @return La lista con elem interpuesto
    */
   def intersperse[A](elem:A, lst:List[A]):List[A] = {
      /**
       * Esta función interna se encarga de ir agregando los valores a una nueva lista
       * @param elem Valor a mezclar
       * @param lst  Lista original
       * @param acum Lista auxiliar
       * @return Nueva lista
       */
      @tailrec
      def intersperseInterno(elem:A, lst:List[A], acum:List[A]):List[A] = (elem,lst) match {
         case (_,Nil) => acum
         case (elem,Const(h,t)) => if(t != Nil) intersperseInterno(elem, t, addEnd(append(acum,List(h)),elem)) else append(acum,List(h))
      }
      intersperseInterno(elem, lst, Nil)
   }

   /**
    * Recibe una lista de lista valores de un tipo A y la transforma en una lista de valores de tipo A
    * @param lst Lista
    * @return Lista concatenada
    */
   def concat[A](lst:List[List[A]]):List[A] = {
      /**
       * Esta función que agrupa las listas para convertirlas en una sola
       * @param lst Lista original (compuesta o no)
       * @param acum Lista auxiliar
       * @return Nueva lista concatenada
       */
      @tailrec
      def concatInterno(lst:List[List[A]], acum:List[A]): List[A] = lst match {
         case Nil => acum
         case Const(h,t) => concatInterno(t,append(acum,h))
      }
      concatInterno(lst, Nil)
   }

   /**
    * Esta función filtra la lista según la condición que se le envíe en la otra función a ejecutar
    * @param lst Lista
    * @param z Valor a calcular
    * @param f Función resultante
    * @return Nueva lista computada
    */
   def reduce(lst:List[Int],z:Int)(f:(Int,Int)=>Int):Int = lst match {
      case Nil => z
      case Const(h,t) => f(h, reduce(t,z)(f))
   }

   def sumaReduce(lst:List[Int]) = reduce(lst,0)((x,y) => x + y)

   def productoReduce(lst:List[Int]) = reduce(lst,1)((x,y) => x * y)

   /**
    * Función que computa otras funciones pero lee los datos desde la derecha hacia la izquierda (final - inicio).
    * No tiene recursividad de cola
    * @param as Lista
    * @param z Valor a usar para operar
    * @param f Función a ejecutar (Recibe dos parámetros)
    * @return Nueva función computada
    */
   def foldRight[A,B](as:List[A], z:B)(f: (A,B) => B):B = as match {
      case Nil => z
      case Const(h,t) => f(h,foldRight(t,z)(f))
   }

   def sumaFR(lst:List[Int]) = foldRight(lst,0)((x,y) => x + y)
   def sumaFROptimizada(lst:List[Int]) = foldRight(lst,0)(_+_)

   def productoFR(lst:List[Int]) = foldRight(lst,1)((x,y) => x * y)
   def productoFROptimizada(lst:List[Int]) = foldRight(lst,1)(_*_)

   def lenghtFR[A](lst:List[A]):Int = foldRight(lst,0)((x,y) => 1 + y)

   def sumarUnoFR(lst:List[Int]):List[Int] = foldRight(lst,Nil:List[Int])((elem,lst) => Const(elem + 1, lst))

   def map[A,B](l: List[A])(f: A => B): List[B] = foldRight(l, Nil:List[B])((h,t) => Const(f(h),t))

   /**
    * Función que computa otras funciones leyendo los datos desde la izquierda a derecha (inicio - final).
    * @param lst Lista
    * @param z Valor a usar para operar
    * @param f Función a ejecutar (Recibe dos parámetros)
    * @return Nueva función computada
    */
   @tailrec
   def foldLeft[A,B](lst:List[A],z:B)(f:(B,A) => B):B = lst match {
      case Nil => z
      case Const(h,t) => foldLeft(t,f(z,h))(f)
   }

   def sumaL(lst:List[Int]) = foldLeft(lst,0)(_+_)
   def productoL(lst:List[Int]) = foldLeft(lst,0)(_*_)






  // def sumarUnoFL(lst:List[Int]):List[Int] =
   //   foldLeft(lst,Nil)((lst,elem) => elem + 1)
}

