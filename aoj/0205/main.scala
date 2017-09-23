import java.util.Scanner

object Main {
  def solve(sc: => Scanner): Unit = {
    val N = sc.nextInt
    if (N != 0) {
      val A = List(N) ++ List.fill[Int](4)(sc.nextInt)
      if (A.groupBy(x => x).size == 2) {
        if (A.filter(_ == 1).size == 0) {
          A.map(a => a - 1).map(x => println(x))
        } else if (A.filter(_ == 2).size == 0) {
          A.map(a => if (a == 3) (1) else (2)).map(x => println(x))
        } else {
          A.map(x => println(x))
        }
      } else {
        A.map(_ => println(3))
      }
      solve(sc)
    }
  }

  def main(args: Array[String]): Unit = {
    val sc: Scanner = new Scanner(System.in)
    solve(sc)
  }
}
