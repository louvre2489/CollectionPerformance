import scala.collection.immutable._
import scala.util.Random

trait SetPerformance extends PerformanceSupport {

  def setAdd(): Unit = {

    println(s"データ追加 HashSet vs ListSet vs TreeSet")

    // 処理時間格納のリスト
    var hashResult = List.empty[Long]
    var listResult = List.empty[Long]
    var treeResult = List.empty[Long]

    // 空コレクションデータ
    val eHash = HashSet.empty[Int]
    val eList = ListSet.empty[Int]
    val eTree = TreeSet.empty[Int]

    println(s"対象件数: ${SIZE_10000}件")

    hashResult = List.empty[Long]
    listResult = List.empty[Long]
    treeResult = List.empty[Long]

    for (i <- 1 to 100) {
      hashResult =
        hashResult.+:(printExecTime(addSetProc(eHash)(SIZE_10000))(i))
      listResult =
        listResult.+:(printExecTime(addSetProc(eList)(SIZE_10000))(i))
      treeResult =
        treeResult.+:(printExecTime(addSetProc(eTree)(SIZE_10000))(i))
    }

    printAverage(
      "HashSet",
      hashResult,
      "ListSet",
      listResult,
      "TreeSet",
      treeResult
    )

    println(s"対象件数: ${SIZE_100000}件")

    hashResult = List.empty[Long]
    listResult = List.empty[Long]
    treeResult = List.empty[Long]

    // ListSetが遅すぎるので、計測を10回に減らす
    for (i <- 1 to 10) {
      hashResult =
        hashResult.+:(printExecTime(addSetProc(eHash)(SIZE_100000))(i))
      listResult =
        listResult.+:(printExecTime(addSetProc(eList)(SIZE_100000))(i))
      treeResult =
        treeResult.+:(printExecTime(addSetProc(eTree)(SIZE_100000))(i))
    }

    printAverage(
      "HashSet",
      hashResult,
      "ListSet",
      listResult,
      "TreeSet",
      treeResult
    )
  }

  def setDataAccess() = {

    println(s"データアクセス HashSet vs ListSet vs TreeSet")

    // 処理時間格納のリスト
    var hashResult = List.empty[Long]
    var listResult = List.empty[Long]
    var treeResult = List.empty[Long]

    // 空コレクションデータ
    val eHash = HashSet.empty[Int]
    val eList = ListSet.empty[Int]
    val eTree = TreeSet.empty[Int]

    // アクセス性能検証用データ
    val hashSet_10000 = addSetProc(eHash)(SIZE_10000)
    val listSet_10000 = addSetProc(eList)(SIZE_10000)
    val treeSet_10000 = addSetProc(eTree)(SIZE_10000)
    val hashSet_100000 = addSetProc(eHash)(SIZE_100000)
    val treeSet_100000 = addSetProc(eTree)(SIZE_100000)
    val hashSet_1000000 = addSetProc(eHash)(SIZE_1000000)
    val treeSet_1000000 = addSetProc(eTree)(SIZE_1000000)

    println(s"対象件数: ${SIZE_10000}件 対象キー：最初に登録した要素")

    hashResult = List.empty[Long]
    listResult = List.empty[Long]
    treeResult = List.empty[Long]

    for (i <- 1 to 100) {
      hashResult = hashResult.+:(printExecTime(dataAccess(hashSet_10000)(1))(i))
      listResult = listResult.+:(printExecTime(dataAccess(listSet_10000)(1))(i))
      treeResult = treeResult.+:(printExecTime(dataAccess(treeSet_10000)(1))(i))
    }

    printAverage(
      "HashSet",
      hashResult,
      "ListSet",
      listResult,
      "TreeSet",
      treeResult
    )

    println(s"対象件数: ${SIZE_10000}件 対象キー：中間に登録した要素")

    hashResult = List.empty[Long]
    listResult = List.empty[Long]
    treeResult = List.empty[Long]

    for (i <- 1 to 100) {
      hashResult = hashResult.+:(
        printExecTime(dataAccess(hashSet_10000)(SIZE_10000 / 2))(i)
      )
      listResult = listResult.+:(
        printExecTime(dataAccess(listSet_10000)(SIZE_10000 / 2))(i)
      )
      treeResult = treeResult.+:(
        printExecTime(dataAccess(treeSet_10000)(SIZE_10000 / 2))(i)
      )
    }

    printAverage(
      "HashSet",
      hashResult,
      "ListSet",
      listResult,
      "TreeSet",
      treeResult
    )

    println(s"対象件数: ${SIZE_10000}件 対象キー：最後に登録した要素")

    hashResult = List.empty[Long]
    listResult = List.empty[Long]
    treeResult = List.empty[Long]

    for (i <- 1 to 100) {
      hashResult =
        hashResult.+:(printExecTime(dataAccess(hashSet_10000)(SIZE_10000))(i))
      listResult =
        listResult.+:(printExecTime(dataAccess(listSet_10000)(SIZE_10000))(i))
      treeResult =
        treeResult.+:(printExecTime(dataAccess(treeSet_10000)(SIZE_10000))(i))
    }

    printAverage(
      "HashSet",
      hashResult,
      "ListSet",
      listResult,
      "TreeSet",
      treeResult
    )

    println(s"対象件数: ${SIZE_100000}件 対象キー：最初に登録した要素")

    hashResult = List.empty[Long]
    treeResult = List.empty[Long]

    for (i <- 1 to 100) {
      hashResult = hashResult.+:(printExecTime(dataAccess(hashSet_100000)(1))(i))
      treeResult = treeResult.+:(printExecTime(dataAccess(treeSet_100000)(1))(i))
    }

    printAverage("HashSet", hashResult, "TreeSet", treeResult)

    println(s"対象件数: ${SIZE_100000}件 対象キー：中間に登録した要素")

    hashResult = List.empty[Long]
    treeResult = List.empty[Long]

    for (i <- 1 to 100) {
      hashResult = hashResult.+:(
        printExecTime(dataAccess(hashSet_100000)(SIZE_100000 / 2))(i)
      )
      treeResult = treeResult.+:(
        printExecTime(dataAccess(treeSet_100000)(SIZE_100000 / 2))(i)
      )
    }

    printAverage("HashSet", hashResult, "TreeSet", treeResult)

    println(s"対象件数: ${SIZE_100000}件 対象キー：最後に登録した要素")

    hashResult = List.empty[Long]
    treeResult = List.empty[Long]

    for (i <- 1 to 100) {
      hashResult =
        hashResult.+:(printExecTime(dataAccess(hashSet_100000)(SIZE_100000))(i))
      treeResult =
        treeResult.+:(printExecTime(dataAccess(treeSet_100000)(SIZE_100000))(i))
    }

    printAverage("HashSet", hashResult, "TreeSet", treeResult)
  }

  def setSequentialAccess() = {}

  def hashSetBitSet() ={}

  def addSetProc(set: Set[Int])(size: Int): Set[Int] = {
    var s = set
    for (i <- 1 to size) {
      s = s.+(i)
    }
    s
  }

  def dataAccess(set: Set[Int])(target: Int): Int = {
    var sum = 0
    for (i <- 1 until set.size) {
      // データの有無を確認した上で加算する
      sum = sum + (if (set.contains(target)) target else 0)
    }
    sum
  }
}
