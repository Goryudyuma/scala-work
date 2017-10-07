import java.util.Scanner

object Main {
  def solve(sc: => Scanner): Unit = {
    val N = sc.nextInt
    var S = sc.next.dropWhile(_=='o')
    while (S.length > 0 && !(S.length >= 2 && S(0) == 'x' && S(1) == 'x')) {
      S = S.drop(1).dropWhile(_=='o')
    }
    println(math.min(N - S.length + 1, N))
  }


  def main(args: Array[String]): Unit = {
    val sc: Scanner = new Scanner(System.in)

    solve(sc)
  }
}
