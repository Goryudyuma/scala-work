//import java.util.Scanner

import scala.collection.Searching._
import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.immutable._
import scala.io.StdIn.readLine

import java.io._
import java.nio.file.{Files, Path}
import java.util.StringTokenizer

import scala.io.Codec

/**
  * Scala implementation of a faster java.util.Scanner
  * See: http://codeforces.com/blog/entry/7018
  */

class Scanner(reader: LineNumberReader) extends Iterable[String] with AutoCloseable {
  def this(reader: BufferedReader) = this(new LineNumberReader(reader))

  def this(reader: Reader) = this(new BufferedReader(reader))

  def this(inputStream: InputStream)(implicit codec: Codec) = this(new InputStreamReader(inputStream, codec.charSet))

  def this(path: Path)(implicit codec: Codec) = this(Files.newBufferedReader(path, codec.charSet))

  def this(file: File)(implicit codec: Codec) = this(file.toPath)(codec)

  def this(str: String) = this(new StringReader(str))

  override def iterator = for {
    line <- Iterator.continually(reader.readLine()).takeWhile(_ != null)
    tokenizer = new StringTokenizer(line)
    tokens <- Iterator.continually(tokenizer).takeWhile(_.hasMoreTokens)
  } yield tokens.nextToken()

  private[this] var current = iterator

  def hasNext = current.hasNext

  @inline def next() = current.next()

  /**
    * This is different from Java's scanner.nextLine
    * The Java one is a misnomer since it actually travel to end of current line
    * This one actually does fetch the next line
    */
  def nextLine(): String = {
    val line = reader.readLine()
    current = iterator
    line
  }

  def lineNumber: Int = reader.getLineNumber

  def nextString(): String = next()

  def nextBoolean(): Boolean = next().toBoolean

  def nextByte(radix: Int = 10): Byte = java.lang.Byte.parseByte(next(), radix)

  def nextShort(radix: Int = 10): Short = java.lang.Short.parseShort(next(), radix)

  def nextInt(radix: Int = 10): Int = java.lang.Integer.parseInt(next(), radix)

  def nextLong(radix: Int = 10): Long = java.lang.Long.parseLong(next(), radix)

  def nextBigInt(radix: Int = 10): BigInt = BigInt(next(), radix)

  def nextFloat(): Float = next().toFloat

  def nextDouble(): Double = next().toDouble

  def nextBigDecimal(): BigDecimal = BigDecimal(next())

  override def close() = reader.close()
}


class IUnionFind(val size: Int) {

  private case class Node(var parent: Option[Int], var treeSize: Int)

  private val nodes = Array.fill[Node](size)(new Node(None, 1))

  def union(t1: Int, t2: Int): IUnionFind = {
    if (t1 == t2) return this

    val root1 = root(t1)
    val root2 = root(t2)
    if (root1 == root2) return this

    val node1 = nodes(root1)
    val node2 = nodes(root2)

    if (node1.treeSize < node2.treeSize) {
      node1.parent = Some(t2)
      node2.treeSize += node1.treeSize
    } else {
      node2.parent = Some(t1)
      node1.treeSize += node2.treeSize
    }
    this
  }

  def connected(t1: Int, t2: Int): Boolean = t1 == t2 || root(t1) == root(t2)

  def root(t: Int): Int = nodes(t).parent match {
    case None => t
    case Some(p) => {
      nodes(t).parent = Some(root(p))
      nodes(t).parent.get
    }
  }
}

object Main {
  def solve(sc: => Scanner): Unit = {
    println(100 - (100 / sc.nextLong()).toInt)
  }

  def calc(N: Long, R: Long, M: Long): Double = {
    math.sqrt(R * R - (R * 2 * ((M * 1.0 / N) - 0.5)) * (R * 2 * ((M * 1.0 / N) - 0.5))) * 2
  }

  def recursive(X: Long, primeList: Stream[Long]): Long = {
    0
  }

  def check(A: Long): Boolean = {
    if (A < 0) (check(-A)) else if (A % 10 == 0) (check(A / 10)) else (A < 10)
  }

  def shift(n: Long): Long = {
    if (n == 0) (0) else if (n == 1) (1) else (shift(n - 1) << 1)
  }

  def unShift(n: Long): Long = {
    if (n == 0) (0) else (unShift(n >> 1) + 1)
  }

  def gcd(i: Long, j: Long): Long = {
    if (i < j) (gcd(j, i)) else (if (j == 0) (i) else (gcd(j, i % j)))
  }

  def main(args: Array[String]): Unit = {
    val sc: Scanner = new Scanner(System.in)
    solve(sc)
  }
}

object Util {
  def getPermutation(begin: Long = 0): Stream[Long] =
    Stream.cons(begin, getPermutation(begin + 1))

  def getPrimeList(): Stream[Long] =
    getPrimeListRecursive(getPermutation(begin = 2))

  private def getPrimeListRecursive(A: Stream[Long]): Stream[Long] =
    Stream.cons(A.head, getPrimeListRecursive(A.tail.filter(_ % A.head != 0)))

  def fib(a: Long = 0, b: Long = 1, mod: Long = Long.MaxValue): Stream[Long] = a #:: fib(b % mod, (a + b) % mod, mod)
}

object ArabicRoman {

  type =?>[A, B] = PartialFunction[A, B]

  val codeTable = List(
    (1000, "M"), (900, "CM"), (500, "D"), (400, "CD"), (100, "C"),
    (90, "XC"), (50, "L"), (40, "XL"), (10, "X"), (9, "IX"), (5, "V"), (4, "IV"), (1, "I"))

  val arabicToRoman: (Int) =?> String = {
    case src if (src >= 1 && src <= 3999) => {

      def convert(left: Int, cont: String = "", code: List[(Int, String)] = codeTable): String = {
        val (unitVal, unitChar) = code.head
        left - unitVal match {
          case n if (n == 0) => cont + unitChar
          case n if (n > 0) => convert(n, cont + unitChar, code)
          case _ => convert(left, cont, code.tail)
        }
      }

      convert(src)
    }
  }

  val romanToArabic: (String) =?> Int = {
    case src if (Option(src).exists { s => {
      s.nonEmpty && ("""[^MDCLXVI]""".r.findFirstMatchIn(s.toUpperCase) == None)
    }
    }) => {

      def convert(left: String, cont: Int = 0, code: List[(Int, String)] = codeTable): Int = {
        val (unitVal, unitChar) = code.head
        left.splitAt(unitChar.length) match {
          case ("", _) => cont
          case (`unitChar`, tail) => convert(tail, cont + unitVal, code)
          case _ => convert(left, cont, code.tail)
        }
      }

      convert(src.toUpperCase())
    }
  }
}
