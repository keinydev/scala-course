package co.s4n.inmutable.companionObjects

/***
 * Ejemplo de Objetos de compañía desarrollado en clase
 * @param value
 * @param step
 */
class Counter(val value:Int =1, val step:Int=1){

  def doStep():Counter = copy(value + step, step)
  def doStep(step:Int):Counter = copy(value + step, step)

  override def toString: String = s"value: $value step: $step"

  def copy(value:Int = this.value, step:Int = this.step) = new Counter(value,step)
}

object Counter {

  def apply(value:Int, step:Int) = new Counter(value, step)

  def main(args:Array[String]):Unit = {
    val c:Counter = Counter(1,1)
    val c2:Counter = Counter(2,2)

    println(c.doStep(1).doStep(2).doStep(3))
    println(c2.doStep(1).doStep(2).doStep(3))
  }
}

//val cont = new Counter(1,1)
//cont.doStep(3).doStep(2).doStep(1)
//cont.copy()