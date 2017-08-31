import java.util.Scanner

object Main {
  def solve(sc: => Scanner): Unit = {
    var data = List.empty[(Double, Double)]
    while (sc.hasNext) {
      data :+= (sc.nextDouble(), sc.nextDouble())
    }
    println(data.filter(a => a._1 >= 1.1).length + " " + data.filter(a => a._2 >= 1.1).length)
    println(data.filter(a => 1.1 > a._1 && a._1 >= 0.6).length + " " + data.filter(a => 1.1 > a._2 && a._2 >= 0.6).length)
    println(data.filter(a => 0.6 > a._1 && a._1 >= 0.2).length + " " + data.filter(a => 0.6 > a._2 && a._2 >= 0.2).length)
    println(data.filter(a => 0.2 > a._1).length + " " + data.filter(a => 0.2 > a._2).length)
  }

  def main(args: Array[String]): Unit = {
    val sc: Scanner = new Scanner(System.in)
    solve(sc)
  }
}
