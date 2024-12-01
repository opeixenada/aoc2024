import scala.annotation.tailrec
import scala.io.Source

object Util {
  def readFile(filename: String): List[String] = {
    val bufferedSource = Source.fromFile(filename)
    val result = bufferedSource.getLines.toList
    bufferedSource.close
    result
  }
  
  def digitToInt(char: Char): Int = char - '0'

  def withTimeLogging[A](block: => A): A = {
    val startTime = System.currentTimeMillis()
    val result = block
    val endTime = System.currentTimeMillis()
    val executionTime = endTime - startTime
    println(s"Execution Time: $executionTime ms")
    result
  }

  @tailrec
  def splitBy[A](xs: List[A], splitCriterion: A => Boolean, acc: List[List[A]] = Nil): List[List[A]] =
    xs match
      case Nil => acc
      case ys =>
        val element = ys.takeWhile(!splitCriterion(_))
        splitBy(xs.drop(element.length + 1), splitCriterion, acc.appended(element))
}
