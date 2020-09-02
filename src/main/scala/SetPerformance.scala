import scala.collection.immutable._
import scala.util.Random

trait SetPerformance extends PerformanceSupport {

  def setAdd(): Unit = {

    println(s"データ追加 HashSet vs ListSet vs TreeSet vs BitSet")

    // 処理時間格納のリスト
    var hashResult = List.empty[Long]
    var listResult = List.empty[Long]
    var treeResult = List.empty[Long]
    var bitResult = List.empty[Long]

    // 空コレクションデータ
    val eHash = HashSet.empty[Int]
    val eList = ListSet.empty[Int]
    val eTree = TreeSet.empty[Int]
    val eBit = BitSet.empty

    println(s"対象件数: ${SIZE_10000}件")

    hashResult = List.empty[Long]
    listResult = List.empty[Long]
    treeResult = List.empty[Long]
    bitResult = List.empty[Long]

    for (i <- 1 to 100) {
      hashResult = printExecTime(addSetProc(eHash)(SIZE_10000))(i) :: hashResult
      listResult = printExecTime(addSetProc(eList)(SIZE_10000))(i) :: listResult
      treeResult = printExecTime(addSetProc(eTree)(SIZE_10000))(i) :: treeResult
      bitResult = printExecTime(addSetProc(eBit)(SIZE_10000))(i) :: bitResult
    }

    printAverage(
      "HashSet",
      hashResult,
      "ListSet",
      listResult,
      "TreeSet",
      treeResult,
      "BitSet",
      bitResult
    )

    println(s"対象件数: ${SIZE_100000}件")

    hashResult = List.empty[Long]
    listResult = List.empty[Long]
    treeResult = List.empty[Long]
    bitResult = List.empty[Long]

    // ListSetが遅すぎるので、計測を10回に減らす
    for (i <- 1 to 10) {
      hashResult = printExecTime(addSetProc(eHash)(SIZE_100000))(i) :: hashResult
      listResult = printExecTime(addSetProc(eList)(SIZE_100000))(i) :: listResult
      treeResult = printExecTime(addSetProc(eTree)(SIZE_100000))(i) :: treeResult
      bitResult = printExecTime(addSetProc(eBit)(SIZE_100000))(i) :: bitResult
    }

    printAverage(
      "HashSet",
      hashResult,
      "ListSet",
      listResult,
      "TreeSet",
      treeResult,
      "BitSet",
      bitResult
    )
  }

  def setDataAccess() = {

    println(s"データアクセス HashSet vs ListSet vs TreeSet vs BitSet")

    // 処理時間格納のリスト
    var hashResult = List.empty[Long]
    var listResult = List.empty[Long]
    var treeResult = List.empty[Long]
    var bitResult = List.empty[Long]

    // 空コレクションデータ
    val eHash = HashSet.empty[Int]
    val eList = ListSet.empty[Int]
    val eTree = TreeSet.empty[Int]
    val eBit = BitSet.empty

    // アクセス性能検証用データ
    val hashSet_10000 = addSetProc(eHash)(SIZE_10000)
    val listSet_10000 = addSetProc(eList)(SIZE_10000)
    val treeSet_10000 = addSetProc(eTree)(SIZE_10000)
    val bitSet_10000 = addSetProc(eBit)(SIZE_10000)
    val hashSet_100000 = addSetProc(eHash)(SIZE_100000)
    val treeSet_100000 = addSetProc(eTree)(SIZE_100000)
    val bitSet_100000 = addSetProc(eBit)(SIZE_100000)
    val hashSet_1000000 = addSetProc(eHash)(SIZE_1000000)
    val treeSet_1000000 = addSetProc(eTree)(SIZE_1000000)
    val bitSet_1000000 = addSetProc(eBit)(SIZE_1000000)

    println(s"対象件数: ${SIZE_10000}件 対象キー：最初に登録した要素")

    hashResult = List.empty[Long]
    listResult = List.empty[Long]
    treeResult = List.empty[Long]
    bitResult = List.empty[Long]

    for (i <- 1 to 100) {
      hashResult = printExecTime(dataAccess(hashSet_10000)(1))(i) :: hashResult
      listResult = printExecTime(dataAccess(listSet_10000)(1))(i) :: listResult
      treeResult = printExecTime(dataAccess(treeSet_10000)(1))(i) :: treeResult
      bitResult = printExecTime(dataAccess(bitSet_10000)(1))(i) :: bitResult
    }

    printAverage(
      "HashSet",
      hashResult,
      "ListSet",
      listResult,
      "TreeSet",
      treeResult,
      "BitSet",
      bitResult
    )

    println(s"対象件数: ${SIZE_10000}件 対象キー：中間に登録した要素")

    hashResult = List.empty[Long]
    listResult = List.empty[Long]
    treeResult = List.empty[Long]
    bitResult = List.empty[Long]

    for (i <- 1 to 100) {
      hashResult =
        printExecTime(dataAccess(hashSet_10000)(SIZE_10000 / 2))(i) :: hashResult
      listResult =
        printExecTime(dataAccess(listSet_10000)(SIZE_10000 / 2))(i) :: listResult
      treeResult =
        printExecTime(dataAccess(treeSet_10000)(SIZE_10000 / 2))(i) :: treeResult
      bitResult =
        printExecTime(dataAccess(bitSet_10000)(SIZE_10000 / 2))(i) :: bitResult
    }

    printAverage(
      "HashSet",
      hashResult,
      "ListSet",
      listResult,
      "TreeSet",
      treeResult,
      "BitSet",
      bitResult
    )

    println(s"対象件数: ${SIZE_10000}件 対象キー：最後に登録した要素")

    hashResult = List.empty[Long]
    listResult = List.empty[Long]
    treeResult = List.empty[Long]
    bitResult = List.empty[Long]

    for (i <- 1 to 100) {
      hashResult = printExecTime(dataAccess(hashSet_10000)(SIZE_10000))(i) :: hashResult
      listResult = printExecTime(dataAccess(listSet_10000)(SIZE_10000))(i) :: listResult
      treeResult = printExecTime(dataAccess(treeSet_10000)(SIZE_10000))(i) :: treeResult
      bitResult = printExecTime(dataAccess(bitSet_10000)(SIZE_10000))(i) :: bitResult
    }

    printAverage(
      "HashSet",
      hashResult,
      "ListSet",
      listResult,
      "TreeSet",
      treeResult,
      "BitSet",
      bitResult
    )

    println(s"対象件数: ${SIZE_100000}件 対象キー：最初に登録した要素")

    hashResult = List.empty[Long]
    treeResult = List.empty[Long]
    bitResult = List.empty[Long]

    for (i <- 1 to 100) {
      hashResult = printExecTime(dataAccess(hashSet_100000)(1))(i) :: hashResult
      treeResult = printExecTime(dataAccess(treeSet_100000)(1))(i) :: treeResult
      bitResult = printExecTime(dataAccess(bitSet_100000)(1))(i) :: bitResult
    }

    printAverage(
      "HashSet",
      hashResult,
      "TreeSet",
      treeResult,
      "BitSet",
      bitResult
    )

    println(s"対象件数: ${SIZE_100000}件 対象キー：中間に登録した要素")

    hashResult = List.empty[Long]
    treeResult = List.empty[Long]
    bitResult = List.empty[Long]

    for (i <- 1 to 100) {
      hashResult =
        printExecTime(dataAccess(hashSet_100000)(SIZE_100000 / 2))(i) :: hashResult
      treeResult =
        printExecTime(dataAccess(treeSet_100000)(SIZE_100000 / 2))(i) :: treeResult
      bitResult =
        printExecTime(dataAccess(bitSet_100000)(SIZE_100000 / 2))(i) :: bitResult
    }

    printAverage(
      "HashSet",
      hashResult,
      "TreeSet",
      treeResult,
      "BitSet",
      bitResult
    )

    println(s"対象件数: ${SIZE_100000}件 対象キー：最後に登録した要素")

    hashResult = List.empty[Long]
    treeResult = List.empty[Long]
    bitResult = List.empty[Long]

    for (i <- 1 to 100) {
      hashResult =
        printExecTime(dataAccess(hashSet_100000)(SIZE_100000))(i) :: hashResult
      treeResult =
        printExecTime(dataAccess(treeSet_100000)(SIZE_100000))(i) :: treeResult
      bitResult =
        printExecTime(dataAccess(bitSet_100000)(SIZE_100000))(i) :: bitResult
    }

    printAverage(
      "HashSet",
      hashResult,
      "TreeSet",
      treeResult,
      "BitSet",
      bitResult
    )
  }

  def setSequentialAccess() = {

    println(s"シーケンシャルアクセス HashSet vs ListSet vs TreeSet vs BitSet")

    // 処理時間格納のリスト
    var hashResult = List.empty[Long]
    var listResult = List.empty[Long]
    var treeResult = List.empty[Long]
    var bitResult = List.empty[Long]

    // 空コレクションデータ
    val eHash = HashSet.empty[Int]
    val eList = ListSet.empty[Int]
    val eTree = TreeSet.empty[Int]
    val eBit = BitSet.empty

    // アクセス性能検証用データ
    val hashSet_100000 = addSetProc(eHash)(SIZE_100000)
    val listSet_100000 = addSetProc(eList)(SIZE_100000)
    val treeSet_100000 = addSetProc(eTree)(SIZE_100000)
    val bitSet_100000 = addSetProc(eBit)(SIZE_100000)
    val hashSet_500000 = addSetProc(eHash)(500000)
    val listSet_500000 = addSetProc(eList)(500000)
    val treeSet_500000 = addSetProc(eTree)(500000)
    val bitSet_500000 = addSetProc(eBit)(500000)
    val hashSet_1000000 = addSetProc(eHash)(SIZE_1000000)
    val listSet_1000000 = addSetProc(eList)(SIZE_1000000)
    val treeSet_1000000 = addSetProc(eTree)(SIZE_1000000)
    val bitSet_1000000 = addSetProc(eBit)(SIZE_1000000)
    val hashSet_10000000 = addSetProc(eHash)(SIZE_10000000)
//    val listSet_10000000 = addSetProc(eList)(SIZE_10000000)
    val treeSet_10000000 = addSetProc(eTree)(SIZE_10000000)
    val bitSet_10000000 = addSetProc(eBit)(SIZE_10000000)

    println(s"対象件数: ${SIZE_100000}件")

    hashResult = List.empty[Long]
    listResult = List.empty[Long]
    treeResult = List.empty[Long]
    bitResult = List.empty[Long]

    for (i <- 1 to 100) {
      hashResult = printExecTime(sequentialAccess(hashSet_100000))(i) :: hashResult
      listResult = printExecTime(sequentialAccess(listSet_100000))(i) :: listResult
      treeResult = printExecTime(sequentialAccess(treeSet_100000))(i) :: treeResult
      bitResult = printExecTime(sequentialAccess(bitSet_100000))(i) :: bitResult
    }

    printAverage(
      "HashSet",
      hashResult,
      "ListSet",
      listResult,
      "TreeSet",
      treeResult,
      "BitSet",
      bitResult
    )

    println(s"対象件数: 500000件")

    hashResult = List.empty[Long]
    listResult = List.empty[Long]
    treeResult = List.empty[Long]
    bitResult = List.empty[Long]

    for (i <- 1 to 100) {
      hashResult = printExecTime(sequentialAccess(hashSet_500000))(i) :: hashResult
      listResult = printExecTime(sequentialAccess(listSet_500000))(i) :: listResult
      treeResult = printExecTime(sequentialAccess(treeSet_500000))(i) :: treeResult
      bitResult = printExecTime(sequentialAccess(bitSet_500000))(i) :: bitResult
    }

    printAverage(
      "HashSet",
      hashResult,
      "ListSet",
      listResult,
      "TreeSet",
      treeResult,
      "BitSet",
      bitResult
    )

    println(s"対象件数: ${SIZE_1000000}件")

    hashResult = List.empty[Long]
    listResult = List.empty[Long]
    treeResult = List.empty[Long]
    bitResult = List.empty[Long]

    for (i <- 1 to 100) {
      hashResult = printExecTime(sequentialAccess(hashSet_1000000))(i) :: hashResult
      listResult = printExecTime(sequentialAccess(listSet_1000000))(i) :: listResult
      treeResult = printExecTime(sequentialAccess(treeSet_1000000))(i) :: treeResult
      bitResult = printExecTime(sequentialAccess(bitSet_1000000))(i) :: bitResult
    }

    printAverage(
      "HashSet",
      hashResult,
      "ListSet",
      listResult,
      "TreeSet",
      treeResult,
      "BitSet",
      bitResult
    )

    println(s"対象件数: ${SIZE_10000000}件")

    hashResult = List.empty[Long]
    treeResult = List.empty[Long]
    bitResult = List.empty[Long]

    for (i <- 1 to 100) {
      hashResult = printExecTime(sequentialAccess(hashSet_10000000))(i) :: hashResult
      treeResult = printExecTime(sequentialAccess(treeSet_10000000))(i) :: treeResult
      bitResult = printExecTime(sequentialAccess(bitSet_10000000))(i) :: bitResult
    }

    printAverage(
      "HashSet",
      hashResult,
      "TreeSet",
      treeResult,
      "BitSet",
      bitResult
    )
  }

  def addSetProc(set: Set[Int])(size: Int): Set[Int] = {
    var s = set
    for (i <- 1 to size) {
      s = s + i
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

  def sequentialAccess(set: Set[Int]): Int = {
    var sum = 0
    set.foreach { elm =>
      sum = sum + elm
    }
    sum
  }
}
