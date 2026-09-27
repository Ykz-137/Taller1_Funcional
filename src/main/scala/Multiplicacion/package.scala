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

  def splitMultiply(a: Int,b: Int): Int = {
    if(a == 0 && b == 0) a*b
    val m = (math.max(math.log10(a).toInt,math.log10(b).toInt)+1)/2
    if(m<=0)
      a*b
    else {
      val x = a/math.pow(10,m).toInt
      val y = a%math.pow(10,m).toInt
      val z = b/math.pow(10,m).toInt
      val w = b%math.pow(10,m).toInt
      math.pow(10,m+m).toInt*splitMultiply(x,z) + math.pow(10,m).toInt*(splitMultiply(y,z)+splitMultiply(x,w)) + splitMultiply(y,w)
    }
  }

  def fastMultiply(a: Int, b: Int): Int = {
    if (a == 0 && b == 0) a * b
    val m = (math.max(math.log10(a).toInt, math.log10(b).toInt) + 1) / 2
    if (m <= 0)
      a * b
    else {
      val x = a / math.pow(10, m).toInt
      val y = a % math.pow(10, m).toInt
      val z = b / math.pow(10, m).toInt
      val w = b % math.pow(10, m).toInt
      math.pow(10, m + m).toInt * fastMultiply(x, z) + math.pow(10,m).toInt*(fastMultiply(x+y,z+w) - fastMultiply(x,z) - fastMultiply(y, w))  + fastMultiply(y, w)
    }
  }

}