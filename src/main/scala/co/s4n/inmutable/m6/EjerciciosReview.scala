package co.s4n.inmutable.m6

sealed class Account
case class User(name:String, account:Account)
case class Paypal(email:String) extends Account
case class Bitcoin(key:String) extends Account

//Enumeramos las temperaturas
sealed trait Temperature
case object Cold extends Temperature
case object Hot extends Temperature

//Enumeramos las temporadas
sealed trait Season
case object Spring extends Season
case object Summer extends Season
case object Autumn extends Season
case object Winter extends Season

//Enumeramos los colores rgb
sealed trait RGB
case object Red extends RGB
case object Green extends RGB
case object Blue extends RGB

//Enumeramos los meses del año
sealed trait Months
case object Jan extends Months
case object Feb extends Months
case object Mar extends Months
case object Apr extends Months
case object May extends Months
case object Jun extends Months
case object Jul extends Months
case object Aug extends Months
case object Sep extends Months
case object Oct extends Months
case object Nov extends Months
case object Dec extends Months

//case class de figuras que reciben parámetros
sealed trait Shape
case class Circle(radio:Float) extends Shape
case class Rectangle(width:Float, height:Float) extends Shape
case class Triangle(size:Float) extends Shape

sealed trait Expr
case class Lit(value:Int) extends Expr
case class Add(expr1:Expr, expr2:Expr) extends Expr
case class Sub(expr1:Expr, expr2:Expr) extends Expr
case class Mul(expr1:Expr, expr2:Expr) extends Expr
case class Div(expr1:Expr, expr2:Expr) extends Expr

object EjerciciosReview {

  /**
   * Funciones anónimas
   */

  val esDivisibleTres = (x: Int) => x % 3 == 0

  val esPar = (x: Int) => x % 2 == 0

  val max3 = (x: Int, y: Int, z: Int) => if (x >= y && x >= z) x else if (y > z) y else z

  /**
   * Funciones parciales
   * No se tiene en cuenta todos los posibles casos, a contraste de
   * función total que tiene una solución para todos los casos
   */

  /**
   * Esta función calcula si una persona tiene derecho a una muerte digna.
   * Este método es parcial porque no tiene todos los casos necesarios para retornar una respuesta
   * @param edad Edad
   * @return Validación
   */
  def derechoMuerteDigna(edad: Int): Boolean = edad match {
    case x if (x > 14) => true
    case x if (x > 12) => true
    case x if (x < 6) => false
  }

  /**
   * Recursión de Cola
   */

  def factorial(n:Int):Int = {
    @annotation.tailrec
    def factorialInterno(n:Int, a:Int):Int = n match {
      case 0 => a
      case 1 => a
      case n => factorialInterno(n-1, n * a)
    }
    factorialInterno(n,1)
  }

  /**
   * Funciones de alto orden
   */

  /**
   * Ejecuta una función enviada por parámetro entre dos números
   * @param f Función a ejecutar
   * @param a Número 1
   * @param b Número 2
   * @return Resultado de operación entre dos números
   */
  def apply(f:(Int, Int) => Int, a:Int, b:Int) = f(a,b)

  /**
   * Esta función devuelve si true o false dependiendo de la condicion enviada
   * Ejemplo: filtro1((_>_), 12,10) true
   * @param f Función
   * @param a Número 1
   * @param b Número 2
   * @return Evaluación
   */
  def filtro1(f:(Int,Int)=>Boolean,a:Int,b:Int):Boolean = if(f(a,b)) true else false

  /**
   * Esta función retorna el valor a o b dependiendo de la condición
   * Ejemplo: filtro2((_>_), 12,10) = 12
   * @param f Función
   * @param a Número 1
   * @param b Número 2
   * @return Evaluación
   */
  def filtro2(f:(Int,Int)=>Boolean,a:Int,b:Int):Int = if(f(a,b)) a else b

  /**
   * Pattern matching
   * 1. Siempre tener en cuenta que tipo de datos estamos manejando ADENTRO de una lista, hay veces que pueden ser tuplas o varios tipos de datos dentro de una lista.
   * 2. Tener el cuenta el nombre al cual le estamos asignando al match case
   * 3. Sempre crear los casos por defecto
   */

  /**
   * Ejemplo de uso:
   * val pater = User("keiny",Paypal("email"))
   * whatis(pater)
   * @param obj
   * @return
   */
  def whatis(obj: Any):String = obj match {
    case User(name,Paypal(email)) => "La cuenta es paypal y el email es "+email
    case User(name,Bitcoin(key)) => "La cuenta es Bitcoin y el email es "+key
    case _ => "No se conoce el objeto"
  }

  /**
   * Currying
   */

  /**
   * Suma dos numeros.
   * Ejemplo de uso:
   * add(2)(_) Aplicación parcial
   * res1(4) = 6
   * @param x Parámetro 1
   * @param y Parámetro 2 que puede ser parcial
   * @return
   */
  def add(x:Int)(y:Int) = x + y

  /**
   * Tipos de datos algebraicos
   */

  /**
   * Esta función retorna la temperatura dependiendo de una temporada tipo Season
   * @param season Nombre de la temporada
   * @return Retorna si es Hot o Cold
   */
  def temp(season:Season):Temperature = season match {
    case Summer => Hot
    case _ => Cold
  }

  /**
   * Calculamos el area de la figura
   * @param shape Figura que queremos calcular
   * @return Area de la figura
   */
  def area(shape:Shape):Float = shape match {
    case Circle(radio) => math.Pi.toFloat * radio * radio
    case Rectangle(width, height) => width * height
    case Triangle(l) => (math.sqrt(3).toFloat * l * l) / 4
  }

  /**
   * Esta función opera el dato aritmetico
   * @param expr Expresión a generar
   * @return Resultado de operación
   */
  def eval(expr:Expr):Int = expr match {
    case Lit(l) => l
    case Add(left, right) => eval(left) + eval(right)
    case Sub(left, right) => eval(left) - eval(right)
    case Mul(left, right) => eval(left) * eval(right)
    case Div(left, right) => eval(left) / eval(right)
  }

  /**
   * Devuelve la cantidad de operadores usada en la expresion
   * @param expr Expresion
   * @return Cantidad de operadores
   */
  def size(expr:Expr):Int = expr match {
    case Lit(_) => 0
    case Add(left, right) => 1 + size(left) + size(right)
    case Sub(left, right) => 1 + size(left) + size(right)
    case Mul(left, right) => 1 + size(left) + size(right)
    case Div(left, right) => 1 + size(left) + size(right)
  }

  /**
   * Fold
   */

  /**
   * Suma todos los elementos de la lista
   * @param lst Lista
   * @return
   */
  def sum(lst:List[Int]) = lst.fold(0){(z,i) => z + i }

  /**
   * Concatena una lista de string
   * @param lst Lista
   * @return
   */
  def concat(lst:List[String]) = lst.fold(""){(z,i) => z + i }

  /**
   * Option
   */

  /**
   * Convierte un string a un número tipo Option
   * Ejemplo de uso:
   * makeInt("1") =  Some(1)
   * makeInt("Hola mundo") = None
   * Nota: Agregando .get se obtiene el valor en entero sin el some
   * @param s Número
   * @return Nuevo dato
   */
  def makeInt(s:String):Option[Int] = {
    try {
      Some(s.trim.toInt)
    } catch {
      case e:Exception => None
    }
  }
}