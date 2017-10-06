import java.util.Scanner

object Main {
  def solve(sc: => Scanner): Unit = {
    val AB, BC, AQ = sc.nextDouble()
    val AC = math.sqrt(AB * AB + BC * BC)
    val RQ = BC * AQ / AC
    val AR = AB * AQ / AC
    val RB = AB - AR
    val RP = RB * RQ / (RQ + BC)
    val AP = AR + RP
    println(AP)
  }


  def main(args: Array[String]): Unit = {
    val sc: Scanner = new Scanner(System.in)

    solve(sc)
  }
}
