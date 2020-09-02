import scala.collection.immutable._
import scala.util.Random

trait MapPerformance extends PerformanceSupport {

  def hashAdd(): Unit = {

    println(s"データ追加 HashMap vs ListMap vs TreeMap")

    // 処理時間格納のリスト
    var hashResult = List.empty[Long]
    var listResult = List.empty[Long]
    var treeResult = List.empty[Long]

    // 空コレクションデータ
    val eHash = HashMap.empty[Int, Int]
    val eList = ListMap.empty[Int, Int]
    val eTree = TreeMap.empty[Int, Int]

    println(s"対象件数: ${SIZE_10000}件")

    hashResult = List.empty[Long]
    listResult = List.empty[Long]
    treeResult = List.empty[Long]

    for (i <- 1 to 100) {
      hashResult = printExecTime(addMapProc(eHash)(SIZE_10000))(i) :: hashResult
      listResult = printExecTime(addMapProc(eList)(SIZE_10000))(i) :: listResult
      treeResult = printExecTime(addMapProc(eTree)(SIZE_10000))(i) :: treeResult
    }

    printAverage(
      "HashMap",
      hashResult,
      "ListMap",
      listResult,
      "TreeMap",
      treeResult
    )

    println(s"対象件数: ${SIZE_100000}件")

    hashResult = List.empty[Long]
    listResult = List.empty[Long]
    treeResult = List.empty[Long]

    // ListMapが遅すぎるので、計測を10回に減らす
    for (i <- 1 to 10) {
      hashResult = printExecTime(addMapProc(eHash)(SIZE_100000))(i) :: hashResult
      listResult = printExecTime(addMapProc(eList)(SIZE_100000))(i) :: listResult
      treeResult = printExecTime(addMapProc(eTree)(SIZE_100000))(i) :: treeResult
    }

    printAverage(
      "HashMap",
      hashResult,
      "ListMap",
      listResult,
      "TreeMap",
      treeResult
    )
  }

  def mapKeyAccess() = {

    println(s"キーアクセス HashMap vs ListMap vs TreeMap")

    // 処理時間格納のリスト
    var hashResult = List.empty[Long]
    var listResult = List.empty[Long]
    var treeResult = List.empty[Long]

    // 空コレクションデータ
    val eHash = HashMap.empty[Int, Int]
    val eList = ListMap.empty[Int, Int]
    val eTree = TreeMap.empty[Int, Int]

    // アクセス性能検証用データ
    val hashMap_10000 = addMapProc(eHash)(SIZE_10000)
    val listMap_10000 = addMapProc(eList)(SIZE_10000)
    val treeMap_10000 = addMapProc(eTree)(SIZE_10000)
    val hashMap_100000 = addMapProc(eHash)(SIZE_100000)
    val treeMap_100000 = addMapProc(eTree)(SIZE_100000)
    val hashMap_1000000 = addMapProc(eHash)(SIZE_1000000)
    val treeMap_1000000 = addMapProc(eTree)(SIZE_1000000)

    println(s"対象件数: ${SIZE_10000}件 対象キー：最初に登録した要素のキー")

    hashResult = List.empty[Long]
    listResult = List.empty[Long]
    treeResult = List.empty[Long]

    for (i <- 1 to 100) {
      hashResult = printExecTime(keyAccess(hashMap_10000)(1))(i) :: hashResult
      listResult = printExecTime(keyAccess(listMap_10000)(1))(i) :: listResult
      treeResult = printExecTime(keyAccess(treeMap_10000)(1))(i) :: treeResult
    }

    printAverage(
      "HashMap",
      hashResult,
      "ListMap",
      listResult,
      "TreeMap",
      treeResult
    )

    println(s"対象件数: ${SIZE_10000}件 対象キー：中間に登録した要素のキー")

    hashResult = List.empty[Long]
    listResult = List.empty[Long]
    treeResult = List.empty[Long]

    for (i <- 1 to 100) {
      hashResult = printExecTime(keyAccess(hashMap_10000)(SIZE_10000 / 2))(i) :: hashResult
      listResult = printExecTime(keyAccess(listMap_10000)(SIZE_10000 / 2))(i) :: listResult
      treeResult = printExecTime(keyAccess(treeMap_10000)(SIZE_10000 / 2))(i) :: treeResult
    }

    printAverage(
      "HashMap",
      hashResult,
      "ListMap",
      listResult,
      "TreeMap",
      treeResult
    )

    println(s"対象件数: ${SIZE_10000}件 対象キー：最後に登録した要素のキー")

    hashResult = List.empty[Long]
    listResult = List.empty[Long]
    treeResult = List.empty[Long]

    for (i <- 1 to 100) {
      hashResult = printExecTime(keyAccess(hashMap_10000)(SIZE_10000))(i) :: hashResult
      listResult = printExecTime(keyAccess(listMap_10000)(SIZE_10000))(i) :: listResult
      treeResult = printExecTime(keyAccess(treeMap_10000)(SIZE_10000))(i) :: treeResult
    }

    printAverage(
      "HashMap",
      hashResult,
      "ListMap",
      listResult,
      "TreeMap",
      treeResult
    )

    println(s"対象件数: ${SIZE_100000}件 対象キー：最初に登録した要素のキー")

    hashResult = List.empty[Long]
    treeResult = List.empty[Long]

    for (i <- 1 to 100) {
      hashResult = printExecTime(keyAccess(hashMap_100000)(1))(i) :: hashResult
      treeResult = printExecTime(keyAccess(treeMap_100000)(1))(i) :: treeResult
    }

    printAverage("HashMap", hashResult, "TreeMap", treeResult)

    println(s"対象件数: ${SIZE_100000}件 対象キー：中間に登録した要素のキー")

    hashResult = List.empty[Long]
    treeResult = List.empty[Long]

    for (i <- 1 to 100) {
      hashResult = printExecTime(keyAccess(hashMap_100000)(SIZE_100000 / 2))(i) :: hashResult
      treeResult = printExecTime(keyAccess(treeMap_100000)(SIZE_100000 / 2))(i) :: treeResult
    }

    printAverage("HashMap", hashResult, "TreeMap", treeResult)

    println(s"対象件数: ${SIZE_100000}件 対象キー：最後に登録した要素のキー")

    hashResult = List.empty[Long]
    treeResult = List.empty[Long]

    for (i <- 1 to 100) {
      hashResult = printExecTime(keyAccess(hashMap_100000)(SIZE_100000))(i) :: hashResult
      treeResult = printExecTime(keyAccess(treeMap_100000)(SIZE_100000))(i) :: treeResult
    }

    printAverage("HashMap", hashResult, "TreeMap", treeResult)

    println(s"対象件数: ${SIZE_1000000}件 対象キー：最初に登録した要素のキー")

    hashResult = List.empty[Long]
    treeResult = List.empty[Long]

    for (i <- 1 to 100) {
      hashResult = printExecTime(keyAccess(hashMap_1000000)(1))(i) :: hashResult
      treeResult = printExecTime(keyAccess(treeMap_1000000)(1))(i) :: treeResult
    }

    printAverage("HashMap", hashResult, "TreeMap", treeResult)

    println(s"対象件数: ${SIZE_1000000}件 対象キー：中間に登録した要素のキー")

    hashResult = List.empty[Long]
    treeResult = List.empty[Long]

    for (i <- 1 to 100) {
      hashResult =
        printExecTime(keyAccess(hashMap_1000000)(SIZE_1000000 / 2))(i) :: hashResult
      treeResult =
        printExecTime(keyAccess(treeMap_1000000)(SIZE_1000000 / 2))(i) :: treeResult
    }

    printAverage("HashMap", hashResult, "TreeMap", treeResult)

    println(s"対象件数: ${SIZE_1000000}件 対象キー：最後に登録した要素のキー")

    hashResult = List.empty[Long]
    treeResult = List.empty[Long]

    for (i <- 1 to 100) {
      hashResult = printExecTime(keyAccess(hashMap_1000000)(SIZE_1000000))(i) :: hashResult
      treeResult = printExecTime(keyAccess(treeMap_1000000)(SIZE_1000000))(i) :: treeResult
    }

    printAverage("HashMap", hashResult, "TreeMap", treeResult)
  }

  def mapSequentialAccess() = {

    println(s"シーケンシャルアクセス HashMap vs ListMap vs TreeMap")

    // 処理時間格納のリスト
    var hashResult = List.empty[Long]
    var listResult = List.empty[Long]
    var treeResult = List.empty[Long]

    // 空コレクションデータ
    val eHash = HashMap.empty[Int, Int]
    val eList = ListMap.empty[Int, Int]
    val eTree = TreeMap.empty[Int, Int]

    // アクセス性能検証用データ
    val hashMap_100000 = addMapProc(eHash)(SIZE_100000)
    val listMap_100000 = addMapProc(eList)(SIZE_100000)
    val treeMap_100000 = addMapProc(eTree)(SIZE_100000)
    val hashMap_500000 = addMapProc(eHash)(500000)
    val listMap_500000 = addMapProc(eList)(500000)
    val treeMap_500000 = addMapProc(eTree)(500000)
    val hashMap_1000000 = addMapProc(eHash)(SIZE_1000000)
    val treeMap_1000000 = addMapProc(eTree)(SIZE_1000000)
    val hashMap_10000000 = addMapProc(eHash)(SIZE_10000000)
    val treeMap_10000000 = addMapProc(eTree)(SIZE_10000000)

    println(s"対象件数: ${SIZE_100000}件")

    hashResult = List.empty[Long]
    listResult = List.empty[Long]
    treeResult = List.empty[Long]

    for (i <- 1 to 100) {
      hashResult = printExecTime(sequentialAccess(hashMap_100000))(i) :: hashResult
      listResult = printExecTime(sequentialAccess(listMap_100000))(i) :: listResult
      treeResult = printExecTime(sequentialAccess(treeMap_100000))(i) :: treeResult
    }

    printAverage(
      "HashMap",
      hashResult,
      "ListMap",
      listResult,
      "TreeMap",
      treeResult
    )

    println(s"対象件数: 500000件")

    hashResult = List.empty[Long]
    listResult = List.empty[Long]
    treeResult = List.empty[Long]

    for (i <- 1 to 100) {
      hashResult = printExecTime(sequentialAccess(hashMap_500000))(i) :: hashResult
      listResult = printExecTime(sequentialAccess(listMap_500000))(i) :: listResult
      treeResult = printExecTime(sequentialAccess(treeMap_500000))(i) :: treeResult
    }

    printAverage(
      "HashMap",
      hashResult,
      "ListMap",
      listResult,
      "TreeMap",
      treeResult
    )

    println(s"対象件数: ${SIZE_1000000}件")

    hashResult = List.empty[Long]
    treeResult = List.empty[Long]

    for (i <- 1 to 100) {
      hashResult = printExecTime(sequentialAccess(hashMap_1000000))(i) :: hashResult
      treeResult = printExecTime(sequentialAccess(treeMap_1000000))(i) :: treeResult
    }

    printAverage("HashMap", hashResult, "TreeMap", treeResult)

    println(s"対象件数: ${SIZE_10000000}件")

    hashResult = List.empty[Long]
    treeResult = List.empty[Long]

    for (i <- 1 to 100) {
      hashResult = printExecTime(sequentialAccess(hashMap_10000000))(i) :: hashResult
      treeResult = printExecTime(sequentialAccess(treeMap_10000000))(i) :: treeResult
    }

    printAverage("HashMap", hashResult, "TreeMap", treeResult)
  }

  def vectorMapListMap() = {

    println(s"データ追加 ListMap vs VectorMap vs TreeSeqMap")

    // 処理時間格納のリスト
    var listResult = List.empty[Long]
    var vectorResult = List.empty[Long]
    var treeSeqResult = List.empty[Long]

    // 空コレクションデータ
    val eList = ListMap.empty[Int, Int]
    val eVector = VectorMap.empty[Int, Int]
    val eTreeSeq = TreeSeqMap.empty[Int, Int]

    println(s"対象件数: ${SIZE_1000000}件")

    listResult = List.empty[Long]
    vectorResult = List.empty[Long]
    treeSeqResult = List.empty[Long]

    for (i <- 1 to 100) {
      listResult = printExecTime(addMapProc(eList)(SIZE_1000000))(i) :: listResult
      vectorResult = printExecTime(addMapProc(eVector)(SIZE_1000000))(i) :: vectorResult
      treeSeqResult = printExecTime(addMapProc(eTreeSeq)(SIZE_1000000))(i) :: treeSeqResult
    }

    printAverage(
      "ListMap",
      listResult,
      "VectorMap",
      vectorResult,
      "TreeSeqMap",
      treeSeqResult
    )

    println(s"キーアクセス ListMap vs VectorMap vs TreeSeqMap")

    // アクセス性能検証用データ
    val listMap_1000000 = addMapProc(eList)(SIZE_1000000)
    val vectorMap_1000000 = addMapProc(eVector)(SIZE_1000000)
    val treeSeqMap_1000000 = addMapProc(eVector)(SIZE_1000000)

    println(s"対象件数: ${SIZE_1000000}件 対象キー：最初に登録した要素のキー")

    listResult = List.empty[Long]
    vectorResult = List.empty[Long]
    treeSeqResult = List.empty[Long]

    for (i <- 1 to 100) {
      listResult = printExecTime(keyAccess(listMap_1000000)(1))(i) :: listResult
      vectorResult = printExecTime(keyAccess(vectorMap_1000000)(1))(i) :: vectorResult
      treeSeqResult = printExecTime(keyAccess(treeSeqMap_1000000)(1))(i) :: treeSeqResult
    }

    printAverage(
      "ListMap",
      listResult,
      "VectorMap",
      vectorResult,
      "TreeSeqMap",
      treeSeqResult
    )

    println(s"対象件数: ${SIZE_1000000}件 対象キー：中間に登録した要素のキー")

    listResult = List.empty[Long]
    vectorResult = List.empty[Long]
    treeSeqResult = List.empty[Long]

    for (i <- 1 to 100) {
      listResult =
        printExecTime(keyAccess(listMap_1000000)(SIZE_1000000 / 2))(i) :: listResult
      vectorResult =
        printExecTime(keyAccess(vectorMap_1000000)(SIZE_1000000 / 2))(i) :: vectorResult
      treeSeqResult =
        printExecTime(keyAccess(treeSeqMap_1000000)(SIZE_1000000 / 2))(i) :: treeSeqResult
    }

    printAverage(
      "ListMap",
      listResult,
      "VectorMap",
      vectorResult,
      "TreeSeqMap",
      treeSeqResult
    )

    println(s"対象件数: ${SIZE_1000000}件 対象キー：最後に登録した要素のキー")

    listResult = List.empty[Long]
    vectorResult = List.empty[Long]
    treeSeqResult = List.empty[Long]

    for (i <- 1 to 100) {
      listResult =
        printExecTime(keyAccess(listMap_1000000)(SIZE_1000000))(i) :: listResult
      vectorResult =
        printExecTime(keyAccess(vectorMap_1000000)(SIZE_1000000))(i) :: vectorResult
      treeSeqResult =
        printExecTime(keyAccess(treeSeqMap_1000000)(SIZE_1000000))(i) :: treeSeqResult
    }

    printAverage(
      "ListMap",
      listResult,
      "VectorMap",
      vectorResult,
      "TreeSeqMap",
      treeSeqResult
    )

    println(s"シーケンシャルアクセス ListMap vs VectorMap vs TreeSeqMap")

    println(s"対象件数: ${SIZE_1000000}件")

    for (i <- 1 to 100) {
      listResult = printExecTime(sequentialAccess(listMap_1000000))(i) :: listResult
      vectorResult = printExecTime(sequentialAccess(vectorMap_1000000))(i) :: vectorResult
      treeSeqResult = printExecTime(sequentialAccess(treeSeqMap_1000000))(i) :: treeSeqResult
    }

    printAverage(
      "ListMap",
      listResult,
      "VectorMap",
      vectorResult,
      "TreeSeqMap",
      treeSeqResult
    )
  }

  def addMapProc(map: Map[Int, Int])(size: Int): Map[Int, Int] = {
    var m = map
    for (i <- 1 to size) {
      m = m.updated(i, randomInt(SIZE_100000000))
    }
    m
  }

  def keyAccess(map: Map[Int, Int])(key: Int): Int = {
    var sum = 0
    for (i <- 1 until map.size) {
      sum = sum + map(key)
    }
    sum
  }

  def sequentialAccess(map: Map[Int, Int]): Int = {
    var sum = 0
    map.foreach { elm =>
      sum = sum + elm._2
    }
    sum
  }
}
