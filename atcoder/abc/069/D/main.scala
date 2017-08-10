import java.util.Scanner

object Main {
  def solve(sc: => Scanner): Unit = {
    val H, W = sc.nextInt
    val A = Array.fill[Int](sc.nextInt)(sc.nextInt).zipWithIndex
    var B = Array.fill[Int](0)(0)
    for (i <- A) {
      B = B ++ Array.fill[Int](i._1)(i._2 + 1)
    }
    B.grouped(W).zipWithIndex.map { case (arr, idx) => if (idx % 2 == 0) (arr) else (arr.reverse) }.foreach(x => println(x.mkString(" ")))
  }

  def main(args: Array[String]): Unit = {
    val sc: Scanner = new Scanner(System.in)
    solve(sc)
  }
}
