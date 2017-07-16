import java.util.Scanner

/**
  * Created by goryudyuma on 2017/07/16.
  */


object Main {
  def main(args: Array[String]): Unit = {
    val sc = new Scanner(System.in)
    val N = sc.nextInt()
    val K = sc.nextInt()
    val arr: Array[Int] = new Array[Int](N)

    for (i <- 0 until N) {
      arr(i) = sc.nextInt()
    }

    println(arr.sorted.reverse.splitAt(K)._1.sum)
  }
}
