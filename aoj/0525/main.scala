import java.util.Scanner

object Main {
  def check(input: Array[Array[Int]]): Int = {
    Array.tabulate(input(0).length)(
      j => math.max(
        Array.tabulate(input.length)(i => input(i)(j)).filter(k => 0 == k).length,
        Array.tabulate(input.length)(i => input(i)(j)).filter(k => 1 == k).length
      )
    ).sum
  }

  def rev(num: Int, input: Array[Array[Int]]): Array[Array[Int]] = {
    Array.tabulate(input.length)(i => if ((num >> i) % 2 == 1) (Array.tabulate(input(i).length)(j => 1 - input(i)(j))) else (input(i)))
  }

  def make(input: Array[Array[Int]]): Int = {
    var max: Int = 0
    for (i <- 0 until 1 << input.length) {
      max = math.max(max, check(rev(i, input)))
    }
    max
  }

  def solve(sc: Scanner): Boolean = {
    val R: Int = sc.nextInt
    val C: Int = sc.nextInt
    if (R != 0 && C != 0) {
      println(make(Array.fill(R)(Array.fill(C)(sc.nextInt))))

      true
    } else {
      false
    }
  }

  def main(args: Array[String]): Unit = {
    val sc: Scanner = new Scanner(System.in)
    while (solve(sc)) ()
  }
}
