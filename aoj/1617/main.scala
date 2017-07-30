import java.util.Scanner

object Main {

  def solve(sc: => Scanner): Unit = {
    val A = sc.next
    if (A != ".") {
      val B = sc.next
      if (A.equals(B)) (println("IDENTICAL"))
      else {
        val arrA = A.split("\"", -1)
        val arrB = B.split("\"", -1)
        if (arrA.length == arrB.length && arrA.zipWithIndex.filter(i => i._2 % 2 == 0).deep == arrB.zipWithIndex.filter(i => i._2 % 2 == 0).deep) {
          val arrAx = arrA.zipWithIndex.filter(i => i._2 % 2 == 1)
          val arrBx = arrB.zipWithIndex.filter(i => i._2 % 2 == 1)

          var count = 0
          for (i <- 0 until arrAx.length) (if (arrAx(i) != arrBx(i)) count += 1)
          if (count == 1) (println("CLOSE")) else (println("DIFFERENT"))
        } else (println("DIFFERENT"))
      }
      solve(sc)
    }
  }

  def main(args: Array[String]): Unit = {
    val sc: Scanner = new Scanner(System.in)
    solve(sc)
  }
}
