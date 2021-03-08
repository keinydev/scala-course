package co.s4n.inmutable.m6
import scala.annotation.tailrec
import scala.util.Random

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
   * Devuelve el último elemento de una lista usando for loop y foldLeft
   * Precondición: La lista no debe ser vacía
   * @param ls Lista
   * @return El último elemento de la lista
   */
  def lastFor[A](lst: List[A]):A = (for{
    xi <- lst
  } yield ((a:A) => xi)).foldLeft(lst.head)((e,f) => f(e))

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
  def elementAt[A](n: Int, lst: List[A]):A = (n, lst) match {
    case (1, head :: _  ) => head
    case (n, _ :: tail) => elementAt(n - 1, tail)
  }

  /**
   * Devuelve el valor que hay la posición buscada usando for y foldLeft
   * Se usa Option para validar si el indice es igual al contador actual para devolver el valor, de lo contrario
   * devuelve None hasta que lo encuentre, incrementando el contador
   * @param n  Posición
   * @param lst Lista
   * @return El valor en la posición
   */
  def elementAtFor[A](n:Int, lst:List[A]) =(for{
    xi <- lst
  } yield ((t:(Int,Option[A])) => (t._2)  match {
                                            case None => if (t._1 == n) (t._1,Some(xi)) else (t._1+1,None)
                                            case Some(x) => (t._1,Some(x))
                                          }
    )).foldLeft((0,None:Option[A]))((e,f) => f(e))._2.getOrElse(-1)

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
   * Esta función devuelve el total de una lista usando un for loop y foldLeft.
   * Para esto se ejecuta una función que ejecute el conteo de cada iteración
   * @param lst Lista
   * @return Longitud
   */
  def lengthFor[A](lst:List[A]):Int = (for {
    _<- lst
  } yield ((a:Int) => a + 1)).foldLeft(0)((e,f) => f(e))

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

  def reverseFor[A](lst: List[A]): List[A] = ???

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
  def isPalindrome[A](lst: List[A]):Boolean =  lst match {
    case Nil  => true
    case x :: Nil  => true
    case x :: xs  => (x == last(xs) && isPalindrome(xs.init))
  }

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

  //def encodeModified[A](lst:List[A]):List[A] = ???

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

  /**
   * Devuelve una lista con los elementos replicados n veces.
   * Se va restando el iterador la cantidad de veces requerida por cada elemento
   * Precondición: la cantidad debe ser mayor a cero
   * @param lst Lista
   * @param n Cantidad de veces a replicar
   * @return Lista con elementos replicados
   */
  def replicate[A](lst:List[A], n:Int):List[A] = {
    @tailrec
    def replicateInterno(lst:List[A], acum:List[A], num:Int, iter:Int):List[A] = (iter, lst) match {
      case (_, Nil) => acum
      case (0, head :: tail) => replicateInterno(tail, acum, num, num)
      case (n, head :: tail) => replicateInterno(lst, acum ::: List(head), num, iter-1)
    }
    replicateInterno(lst,Nil,n,n)
  }

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

  /**
   * Esta función rota una lista N lugares a la izquierda
   * Si la posición es negativa, se comienza desde la izquierda
   * @param num Indices a rotar
   * @param ls Lista
   * @return Lista nueva
   */
  def rotate[A](num: Int, ls: List[A]): List[A] = {
    @tailrec
    def rotateInterno(num: Int, ls: List[A]): List[A] = (num, ls) match {
      case (_, Nil) => Nil
      case (0, _) => ls
      case (_, head :: tail) => rotateInterno(num - 1, tail ::: List(head))
    }

    val rotations = if (num > 0) num else num + ls.length
    rotateInterno(rotations, ls)
  }

  /**
   * Esta función elimina una posición de la lista. Si el indice llega a cero,se devuelve el head, de lo
   * contrario se va disminuyendo el indice hasta que sea cero, devolviendo el acumulado
   * @param index Index a eliminar
   * @param lst Lista
   * @return
   */
  def removeAt[A](index: Int, lst: List[A]): (List[A], A) = {
    @tailrec
    def removeAtInterno(index: Int, lst: List[A], acum: List[A]): (List[A], A) = (index, lst) match {
      case (0, head :: tail) => (acum ::: tail, head)
      case (index, head :: tail) => removeAtInterno(index - 1, tail, acum ::: List(head))
    }
    removeAtInterno(index, lst, List())
  }

  /**
   * Esta función inserta un elemento en una posición dada en la lista.
   * El indice se va disminuyendo hasta que se se encuentre la posición a insertar el valor en la lista
   * @param value Valor a insertar
   * @param lst Lista
   * @param index Indice donde se quiere insertar el valor
   * @return
   */
  def insertAt[A](value: A, lst: List[A], index: Int): List[A] =  lst match {
    case head :: tail => if (index > 1) head :: insertAt(value, tail, index-1) else value :: lst
    case _ => value :: lst
  }

  /**
   * Crea una lista de enteros con el rango indicado.
   * Si el primer rango es mayor al segundo, se decrementa de lo contrario se va incrementando hasta llegar
   * @param x Primer número
   * @param y Último número
   * @return Lista de rango
   */
  def range(x: Int, y: Int) : List[Int] = x match {
    case x if(x == y) => List(y)
    case x => if(x > y) x :: range(x - 1, y) else x :: range(x + 1, y)
  }

  /**
   * Devuelve n elementos random de una lista.
   * Por cada matching, se está generando un aleatorio con la función Random.nextInt y se extrae este nuevo valor con elementAt para agregarlo a una lista nueva + el acumulado
   * Precondición: n debe ser menor o igual a la cantidad de elementos de la lista.
   * Ejemplo de uso randomSelect(4,List(1,2,3,4,5,6,7,8,9))
   * @param n Cantidad de elementos
   * @param lst Lista
   * @return
   */
  def randomSelect[A](n:Int, lst:List[A]):List[A] = {
    @tailrec
    def randomSelectInterno(n: Int, lst:List[A], acum:List[A]):List[A] = n match {
      case 0 => acum
      case n => randomSelectInterno(n - 1, lst, (elementAt(Random.nextInt(lst.length),lst) :: acum))
    }
    randomSelectInterno(n, lst, Nil)
  }

  /**
   * Dibuja N números aleatorios diferentes del conjunto 1..M.
   * Usamos la función randomSelect para generar la selección de x cantidad, y usamos range para generar el rango entre 1 y "y"
   * Ejemplo de uso lotto(3,50)
   * @param x Cantidad de elementos por lista
   * @param y Tope del rango a generar (de 1 a y)
   * @return
   */
  def lotto[A](x:Int, y:Int):List[Int] = randomSelect(x, range(1, y))

  /**
   * Genera una permutación aleatoria de los elementos de una lista.
   * Usamos randomSelect para generar los elementos aleatorios de la lista.
   * Ejemplo de uso randomPermute(List(1,2,3,4,5,6))
   * @param lst Lista
   * @return Permutación aleatoria
   */
  def randomPermute[A](lst:List[A]):List[A] = randomSelect(lst.length, lst)

  /**
   * Genera una combinaciones de n cantidades de una lista
   * Se va agregando el head + el tail de la iteración restante
   * @param q Cantidad de combinaciones
   * @param lst Lista
   * @return Lista con sublistas de combinaciones
   */
  def combinations[A](q: Int, lst: List[A]): List[List[A]] = lst match {
    case head :: tail if q == 1 => lst.map(List(_))
    case head :: tail => combinations(q - 1, tail).map(head :: _) ::: combinations(q, tail)
    case Nil => Nil
  }

  /**
   * Devuelve el head de una lista usando for y foldRight.
   * Nota: Este es un ejemplo y no hace parte de las preguntas
   * Precondición: La lista no debe ser vacía
   * @param lst Lista
   * @return Head de la lista
   */
  def headFor[A](lst: List[A]):A = (for{
    xi <- lst
  } yield ((a:A) => xi)).foldRight(lst.head)((f,e) => f(e))

  /**
   * Permite copiar una lista usando for y foldRight.
   * Este ejercicio no hace parte de las 99 preguntas
   * @param lst Lista
   * @return Lista copiada
   */
  def copyFor[A](lst:List[A]) = (for {
    xi <- lst
  } yield ((tail:List[A]) => xi :: tail)).foldRight(Nil:List[A])((f,e) => f(e))
}