import java.util.Scanner

object Main {

  def make(N: Int, M: Int, sc: Scanner): String = Array.fill[Array[Int]](N)(Array.fill[Int](M)(sc.nextInt)).foldLeft(Array.fill[Int](M)(0))((acc, arr) => acc.zip(arr).map { case (x, y) => x + y }).zipWithIndex.sortWith((a, b) => (if (a._1 == b._1) (a._2 < b._2) else (a._1 > b._1))).map(a => a._2 + 1).mkString(" ")


  def solve(sc: => Scanner): Unit = {
    val N: Int = sc.nextInt
    val M: Int = sc.nextInt
    if (N != 0 && M != 0) {
      println(make(N, M, sc))
      solve(sc)
    }
  }

  def main(args: Array[String]): Unit = {
    val sc: Scanner = new Scanner(System.in)
    solve(sc)
  }
}
