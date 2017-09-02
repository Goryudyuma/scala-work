import java.util.Scanner

object Main {
  def solve(sc: => Scanner): Unit = {
    val Arr = Array.fill[Int](sc.nextInt)(sc.nextInt).groupBy(c => c).foldLeft[(Long, Long)]((0, 0))((A, B) => (
      if (B._2.length >= 4) ((math.max(A._1, B._1), math.max(A._2, B._1))) else if (B._2.length >= 2) (if (A._1 < A._2) ((math.max(A._1, B._1), A._2)) else ((A._1, math.max(A._2, B._1))))
      else (A)
      ))
    println(Arr._1 * Arr._2)
  }

  def main(args: Array[String]): Unit = {
    val sc: Scanner = new Scanner(System.in)
    solve(sc)
  }
}
