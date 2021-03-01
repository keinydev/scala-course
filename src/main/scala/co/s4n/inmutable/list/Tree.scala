package co.s4n.inmutable.list
sealed trait Tree[+A]
case object Empty extends Tree[Nothing]
case class Leaf[A](value:A) extends Tree[A]
case class Branch[A](left:Tree[A],right:Tree[A]) extends Tree[A]

/**
 * Branch(Branch(Leaf("a"),Leaf("b")), Branch(Leaf("c"),Leaf("d")))
 *
 *                      Branch
 *            ---------|    |--------
 *            |       left right     |
 *         Branch                  Branch
 *    ------|  |------        ------|  |------
 *   |                |      |                |
 *   Leaf           Leaf    Leaf             Leaf
 *   "a"            "b"     "c"              "d"
 *
 */

object Tree {

  /**
   * Esta función cuenta el número de nodos Leaf y Branches en un árbol
   * @param tree
   * @return Cantidad de nodos
   */
  def size[A](tree:Tree[A]):Int = tree match {
    case Leaf(_) => 1
    case Branch(left, right) => 1 + size(left) + size(right)
  }

  /**
   * Retorna la longitud máxima desde profundidad desde la raı́z a cualquier hoja
   * @param tree
   * @return Longitud máxima
   */
  def depth[A](tree: Tree[A]):Int = tree match {
    case Leaf(_) => 1
    case Branch(left, right) => if(depth(left) > depth(right)) 1 + depth(left) else 1 + depth(right)
  }

 // def sumar[A]suma








}