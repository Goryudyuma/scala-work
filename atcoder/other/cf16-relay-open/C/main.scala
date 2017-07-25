import java.util.Scanner
 
object Main {
 
  def solve(sc: => Scanner): Unit = {
    val N = sc.nextInt
 
    val A = Array.fill[Int](1 << N)(sc.nextInt)
    for (i <- 0 until N) {
      for (j <- 0 until 1 << N by 1 << (i + 1)) {
        if (A(j) != A(j + (1 << (i)))) (
          A(j) = math.abs(A(j) - A(j + (1 << (i))))
          )
      }
    }
 
    println(A(0))
  }
 
  def main(args: Array[String]): Unit = {
    val sc: Scanner = new Scanner(System.in)
    solve(sc)
  }
}
