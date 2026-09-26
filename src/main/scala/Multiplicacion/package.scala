import scala.annotation.tailrec

package object Multiplicacion {

  def PeasantAlgorithm(a: Int, b: Int): Int = {
    if (a == 0) 0 else if (a % 2 == 0) PeasantAlgorithm(a / 2, b + b) else b + PeasantAlgorithm(a / 2, b + b)
  }
  
  def PeasantAlgorithmIt(a: Int,b: Int): Int =  PeasantAlgorithmStructureIt(a,b,0)
  @tailrec
  private def PeasantAlgorithmStructureIt(x: Int, y: Int, n: Int): Int = {
    if (x == 0) n else if (x % 2 == 0) PeasantAlgorithmStructureIt(x / 2, y + y,n) else PeasantAlgorithmStructureIt(x / 2, y + y, n+y)
  }
}