import scala.collection.immutable._
import scala.util.Random

trait PerformanceSupport {

  // 個別の処理時間を出力するためのフラグ
  val printout = false

  val SIZE_10000 = 10000
  val SIZE_100000 = 100000
  val SIZE_1000000 = 1000000
  val SIZE_10000000 = 10000000
  val SIZE_100000000 = 100000000

  var RANDOM = new Random

  def printExecTime(process: => Unit)(i: Int): Long = {
    val start = System.currentTimeMillis

    process

    val procTime = System.currentTimeMillis - start

    if (printout) {
      println(s"--- ${i}回目 ---")
      println(s"処理時間： ${procTime} ミリ秒")
    }

    procTime
  }

  def printAverage(p1: String,
                   p1Result: List[Long],
                   p2: String,
                   p2Result: List[Long]): Unit = {
    println(s"${p1}平均処理時間: ${p1Result.sum / p1Result.size} ミリ秒")
    println(s"${p2}平均処理時間: ${p2Result.sum / p2Result.size} ミリ秒")
  }

  def printAverage(p1: String,
                   p1Result: List[Long],
                   p2: String,
                   p2Result: List[Long],
                   p3: String,
                   p3Result: List[Long]): Unit = {
    println(s"${p1}平均処理時間: ${p1Result.sum / p1Result.size} ミリ秒")
    println(s"${p2}平均処理時間: ${p2Result.sum / p2Result.size} ミリ秒")
    println(s"${p3}平均処理時間: ${p3Result.sum / p3Result.size} ミリ秒")
  }

  def printAverage(p1: String,
                   p1Result: List[Long],
                   p2: String,
                   p2Result: List[Long],
                   p3: String,
                   p3Result: List[Long],
                   p4: String,
                   p4Result: List[Long]): Unit = {
    println(s"${p1}平均処理時間: ${p1Result.sum / p1Result.size} ミリ秒")
    println(s"${p2}平均処理時間: ${p2Result.sum / p2Result.size} ミリ秒")
    println(s"${p3}平均処理時間: ${p3Result.sum / p3Result.size} ミリ秒")
    println(s"${p4}平均処理時間: ${p4Result.sum / p4Result.size} ミリ秒")
  }

  def randomInt(upper: Int): Int = {
    RANDOM.nextInt(upper)
  }
}
