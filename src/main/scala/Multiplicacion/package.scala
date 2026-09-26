package object Multiplicacion {
  
  def PeasantAlgorithm(a: Int, b: Int): Int = {
    if (a == 0) 0 else if (a % 2 == 0) PeasantAlgorithm(a / 2, b + b) else b + PeasantAlgorithm(a / 2, b + b)
  }
  
  def PeasantAlgorithmIt(x: Int, y: Int): Int = {
    if (x == 0) 0 else if (x % 2 == 0) PeasantAlgorithmIt(x / 2, y + y) else y + PeasantAlgorithmIt(x / 2, y + y)
  }

}
