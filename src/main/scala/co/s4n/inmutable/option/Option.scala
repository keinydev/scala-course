package co.s4n.inmutable.option

object Option {

  /**
   * Devuelve la media de una lista de doubles.
   * Se usa None en caso de que exista la lista vacía
   * Con Some, se retorna un valor calculado, al existir datos en la lista
   * @param lst Lista
   * @return Promedio
   */
  def media(lst:List[Double]):Option[Double] = {
     if(lst.isEmpty) None
     else Some(lst.sum / lst.length)
  }

  /**
   * Ejecuta operación sobre una lista
   * @param lst Lista
   * @param f Función a ejecutar
   * @return Función ejecutada sobre lista
   */
  def mapa[A,B](lst:List[A])(f: (A) => B):List[B] = {
    for{
      x <- lst
    } yield (f(x))
  }

  /**
   * Esta función devuelve una lista que cumplan con la condición solitada
   * @param lst Lista
   * @param p Predicado verdadero
   * @return Lista validada
   */
  def filtro[A](lst:List[A])(p: (A) => Boolean):List[A] = for {
    x <- lst //generador
    if(p(x))  //guarda
  } yield(x)

  /**
   * Esta función indica si un número divide a otro
   * @param d Dividendo
   * @param n Número
   * @return Validación
   */
  def divideA(d:Int,n:Int):Boolean = n % d == 0

  /**
   * Esta función encuentra los divisores de un número solicitado
   * @param n Número a calcular
   * @return Lista de enteros
   */
  def divisoresDe(n:Int):List[Int] = for {
    x <- List.range(1,n+1)
    if divideA(x,n)
  } yield(x)

  /**
   * Esta función valida si un número es primo
   * @param n Número
   * @return Validación
   */
  def esPrimo(n:Int):Boolean = divisoresDe(n) == List(1,n)
}
