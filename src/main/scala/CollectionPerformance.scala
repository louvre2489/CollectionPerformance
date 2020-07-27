import scala.collection.immutable.{HashMap, ListMap, TreeMap, VectorMap}
import scala.util.Random

object CollectionPerformance extends App {

  // 個別の処理時間を出力するためのフラグ
  val printout = false

  val SIZE_10000 = 10000
  val SIZE_100000 = 100000
  val SIZE_1000000 = 1000000
  val SIZE_10000000 = 10000000
  val SIZE_100000000 = 100000000

  var RANDOM = new Random

  // List
//  listAdd()

  println("----------------------------------------")

//  listRandomAccess()

  println("----------------------------------------")

//  listSequentialAccess()

//  lazyListAdd()

  println("----------------------------------------")

//  lazyListRandomAccess()

  println("----------------------------------------")

//  lazyListSequentialAccess()

  println("----------------------------------------")

  // Map
//  mapAdd()

  println("----------------------------------------")

//  mapKeyAccess()

  println("----------------------------------------")

//  mapSequentialAccess()

  println("----------------------------------------")

  vectorMapListMap()

  // Set

  def listAdd(): Unit = {

    println(s"データ追加 Vector vs List")

    // 処理時間格納のリスト
    var vectorResult = List.empty[Long]
    var listResult = List.empty[Long]

    // 空コレクションデータ
    val eVector = Vector.empty[Int]
    val eList = List.empty[Int]

    println(s"対象件数: ${SIZE_1000000}件")

    vectorResult = List.empty[Long]
    listResult = List.empty[Long]

    for (i <- 1 to 100) {
      vectorResult =
        vectorResult.+:(printExecTime(addProc(eVector)(SIZE_1000000))(i))
      listResult = listResult.+:(printExecTime(addProc(eList)(SIZE_1000000))(i))
    }

    printAverage("Vector", vectorResult, "List", listResult)

    println(s"対象件数: ${SIZE_10000000}件")

    vectorResult = List.empty[Long]
    listResult = List.empty[Long]

    for (i <- 1 to 100) {
      vectorResult =
        vectorResult.+:(printExecTime(addProc(eVector)(SIZE_10000000))(i))
      listResult =
        listResult.+:(printExecTime(addProc(eList)(SIZE_10000000))(i))
    }
    printAverage("Vector", vectorResult, "List", listResult)
  }

  def listRandomAccess(): Unit = {

    println(s"ランダムアクセス Vector vs List")

    // 処理時間格納のリスト
    var vectorResult = List.empty[Long]
    var listResult = List.empty[Long]

    // 空コレクションデータ
    val eVector = Vector.empty[Int]
    val eList = List.empty[Int]

    // 読み込み性能検証用データ
    val vector_10000 = addProc(eVector)(SIZE_10000).toVector
    val list_10000 = addProc(eList)(SIZE_10000).toList
    val vector_100000 = addProc(eVector)(SIZE_100000).toVector
    val list_100000 = addProc(eList)(SIZE_100000).toList

    println(s"対象件数: ${SIZE_10000}件")

    vectorResult = List.empty[Long]
    listResult = List.empty[Long]

    for (i <- 1 to 100) {
      vectorResult =
        vectorResult.+:(printExecTime(randomSumProc(vector_10000))(i))
      listResult = listResult.+:(printExecTime(randomSumProc(list_10000))(i))
    }

    printAverage("Vector", vectorResult, "List", listResult)

    println(s"対象件数: ${SIZE_100000}件")

    vectorResult = List.empty[Long]
    listResult = List.empty[Long]

    // Listのランダムアクセスが遅すぎるので、計測を10回に減らす
    for (i <- 1 to 10) {
      vectorResult =
        vectorResult.+:(printExecTime(randomSumProc(vector_100000))(i))
      listResult = listResult.+:(printExecTime(randomSumProc(list_100000))(i))
    }

    printAverage("Vector", vectorResult, "List", listResult)
  }

  def listSequentialAccess(): Unit = {

    println(s"シーケンシャルアクセス Vector vs List")

    // 処理時間格納のリスト
    var vectorResult = List.empty[Long]
    var listResult = List.empty[Long]

    // 空コレクションデータ
    val eVector = Vector.empty[Int]
    val eList = List.empty[Int]

    // 読み込み性能検証用データ
    val vector_10000 = addProc(eVector)(SIZE_10000).toVector
    val list_10000 = addProc(eList)(SIZE_10000).toList
    val vector_100000 = addProc(eVector)(SIZE_100000).toVector
    val list_100000 = addProc(eList)(SIZE_100000).toList
    val vector_1000000 = addProc(eVector)(SIZE_1000000).toVector
    val list_1000000 = addProc(eList)(SIZE_1000000).toList

    println(s"対象件数: ${SIZE_10000}件")

    vectorResult = List.empty[Long]
    listResult = List.empty[Long]

    for (i <- 1 to 100) {
      vectorResult =
        vectorResult.+:(printExecTime(sequentialSumProc(vector_10000))(i))
      listResult =
        listResult.+:(printExecTime(sequentialSumProc(list_10000))(i))
    }

    printAverage("Vector", vectorResult, "List", listResult)

    println(s"対象件数: ${SIZE_100000}件")

    vectorResult = List.empty[Long]
    listResult = List.empty[Long]

    for (i <- 1 to 100) {
      vectorResult =
        vectorResult.+:(printExecTime(sequentialSumProc(vector_100000))(i))
      listResult =
        listResult.+:(printExecTime(sequentialSumProc(list_100000))(i))
    }

    printAverage("Vector", vectorResult, "List", listResult)

    println(s"対象件数: ${SIZE_1000000}件")

    vectorResult = List.empty[Long]
    listResult = List.empty[Long]

    for (i <- 1 to 100) {
      vectorResult =
        vectorResult.+:(printExecTime(sequentialSumProc(vector_1000000))(i))
      listResult =
        listResult.+:(printExecTime(sequentialSumProc(list_1000000))(i))
    }

    printAverage("Vector", vectorResult, "List", listResult)
  }

  def lazyListAdd(): Unit = {

    println(s"データ追加 LazyList vs List")

    // 処理時間格納のリスト
    var lazyResult = List.empty[Long]
    var listResult = List.empty[Long]

    // 空コレクションデータ
    val eLazy = LazyList.empty[Int]
    val eList = List.empty[Int]

    println(s"対象件数: ${SIZE_1000000}件")

    lazyResult = List.empty[Long]
    listResult = List.empty[Long]

    for (i <- 1 to 100) {
      lazyResult = lazyResult.+:(printExecTime(addProc(eLazy)(SIZE_1000000))(i))
      listResult = listResult.+:(printExecTime(addProc(eList)(SIZE_1000000))(i))
    }

    printAverage("LazyList", lazyResult, "List", listResult)

    println(s"対象件数: ${SIZE_10000000}件")

    lazyResult = List.empty[Long]
    listResult = List.empty[Long]

    for (i <- 1 to 100) {
      lazyResult =
        lazyResult.+:(printExecTime(addProc(eLazy)(SIZE_10000000))(i))
      listResult =
        listResult.+:(printExecTime(addProc(eList)(SIZE_10000000))(i))
    }
    printAverage("LazyList", lazyResult, "List", listResult)
  }

  def lazyListRandomAccess(): Unit = {

    println(s"ランダムアクセス LazyList vs List")

    // 処理時間格納のリスト
    var lazyListResult = List.empty[Long]
    var listResult = List.empty[Long]

    // 空コレクションデータ
    val eLazyList = LazyList.empty[Int]
    val eList = List.empty[Int]

    // 読み込み性能検証用データ
    val lazyList_10000 = addLazyProc(eLazyList)(SIZE_10000)
    val list_10000 = addProc(eList)(SIZE_10000).toList
    val lazyList_100000 = addLazyProc(eLazyList)(SIZE_100000)
    val list_100000 = addProc(eList)(SIZE_100000).toList

    println(s"対象件数: ${SIZE_10000}件")

    lazyListResult = List.empty[Long]
    listResult = List.empty[Long]

    for (i <- 1 to 100) {
      lazyListResult =
        lazyListResult.+:(printExecTime(randomSumProc(lazyList_10000))(i))
      listResult = listResult.+:(printExecTime(randomSumProc(list_10000))(i))
    }

    printAverage("LazyList", lazyListResult, "List", listResult)

    println(s"対象件数: ${SIZE_100000}件")

    lazyListResult = List.empty[Long]
    listResult = List.empty[Long]

    // Listのランダムアクセスが遅すぎるので、計測を10回に減らす
    for (i <- 1 to 10) {
      lazyListResult =
        lazyListResult.+:(printExecTime(randomSumProc(lazyList_100000))(i))
      listResult = listResult.+:(printExecTime(randomSumProc(list_100000))(i))
    }

    printAverage("LazyList", lazyListResult, "List", listResult)
  }

  def lazyListSequentialAccess(): Unit = {

    println(s"シーケンシャルアクセス LazyList vs List")

    // 処理時間格納のリスト
    var lazyListResult = List.empty[Long]
    var listResult = List.empty[Long]

    // 空コレクションデータ
    val eLazyList = LazyList.empty[Int]
    val eList = List.empty[Int]

    // 読み込み性能検証用データ
    val lazyList_10000 = addLazyProc(eLazyList)(SIZE_10000)
    val list_10000 = addProc(eList)(SIZE_10000).toList
    val lazyList_100000 = addLazyProc(eLazyList)(SIZE_100000)
    val list_100000 = addProc(eList)(SIZE_100000).toList
    val lazyList_1000000 = addLazyProc(eLazyList)(SIZE_1000000)
    val list_1000000 = addProc(eList)(SIZE_1000000).toList

    println(s"対象件数: ${SIZE_10000}件")

    lazyListResult = List.empty[Long]
    listResult = List.empty[Long]

    for (i <- 1 to 100) {
      lazyListResult =
        lazyListResult.+:(printExecTime(sequentialSumProc(lazyList_10000))(i))
      listResult =
        listResult.+:(printExecTime(sequentialSumProc(list_10000))(i))
    }

    printAverage("LazyList", lazyListResult, "List", listResult)

    println(s"対象件数: ${SIZE_100000}件")

    lazyListResult = List.empty[Long]
    listResult = List.empty[Long]

    for (i <- 1 to 100) {
      lazyListResult =
        lazyListResult.+:(printExecTime(sequentialSumProc(lazyList_100000))(i))
      listResult =
        listResult.+:(printExecTime(sequentialSumProc(list_100000))(i))
    }

    printAverage("LazyList", lazyListResult, "List", listResult)

    println(s"対象件数: ${SIZE_1000000}件")

    lazyListResult = List.empty[Long]
    listResult = List.empty[Long]

    for (i <- 1 to 100) {
      lazyListResult =
        lazyListResult.+:(printExecTime(sequentialSumProc(lazyList_1000000))(i))
      listResult =
        listResult.+:(printExecTime(sequentialSumProc(list_1000000))(i))
    }

    printAverage("LazyList", lazyListResult, "List", listResult)
  }

  def addProc(seq: Seq[Int])(size: Int): Seq[Int] = {
    var s = seq
    for (_ <- 1 to size) {
      s = s.+:(randomInt(SIZE_100000000))
    }
    s
  }

  def addLazyProc(seq: LazyList[Int])(size: Int): LazyList[Int] = {
    var s = seq
    for (_ <- 1 to size) {
      s = s.+:(randomInt(SIZE_100000000))
    }
    s
  }

  def randomSumProc(seq: Seq[Int]): Int = {
    var sum = 0
    for (i <- 0 until seq.size) {
      sum = sum + seq(i)
    }
    sum
  }

  def sequentialSumProc(seq: Seq[Int]): Int = {
    def loop(list: Seq[Int], sum: Int): Int = {
      // `head :: tail` や `elm :: Nil` のような書き方をすると、Vectorを渡した時に例外が発生する
      list match {
        case _ if list.isEmpty => sum
        case _                 => loop(list.tail, sum + list.head)
      }
    }

    loop(seq, 0)
  }

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
      hashResult =
        hashResult.+:(printExecTime(addMapProc(eHash)(SIZE_10000))(i))
      listResult =
        listResult.+:(printExecTime(addMapProc(eList)(SIZE_10000))(i))
      treeResult =
        treeResult.+:(printExecTime(addMapProc(eTree)(SIZE_10000))(i))
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
      hashResult =
        hashResult.+:(printExecTime(addMapProc(eHash)(SIZE_100000))(i))
      listResult =
        listResult.+:(printExecTime(addMapProc(eList)(SIZE_100000))(i))
      treeResult =
        treeResult.+:(printExecTime(addMapProc(eTree)(SIZE_100000))(i))
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
      hashResult = hashResult.+:(printExecTime(keyAccess(hashMap_10000)(1))(i))
      listResult = listResult.+:(printExecTime(keyAccess(listMap_10000)(1))(i))
      treeResult = treeResult.+:(printExecTime(keyAccess(treeMap_10000)(1))(i))
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
      hashResult = hashResult.+:(
        printExecTime(keyAccess(hashMap_10000)(SIZE_10000 / 2))(i)
      )
      listResult = listResult.+:(
        printExecTime(keyAccess(listMap_10000)(SIZE_10000 / 2))(i)
      )
      treeResult = treeResult.+:(
        printExecTime(keyAccess(treeMap_10000)(SIZE_10000 / 2))(i)
      )
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
      hashResult =
        hashResult.+:(printExecTime(keyAccess(hashMap_10000)(SIZE_10000))(i))
      listResult =
        listResult.+:(printExecTime(keyAccess(listMap_10000)(SIZE_10000))(i))
      treeResult =
        treeResult.+:(printExecTime(keyAccess(treeMap_10000)(SIZE_10000))(i))
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
      hashResult = hashResult.+:(printExecTime(keyAccess(hashMap_100000)(1))(i))
      treeResult = treeResult.+:(printExecTime(keyAccess(treeMap_100000)(1))(i))
    }

    printAverage("HashMap", hashResult, "TreeMap", treeResult)

    println(s"対象件数: ${SIZE_100000}件 対象キー：中間に登録した要素のキー")

    hashResult = List.empty[Long]
    treeResult = List.empty[Long]

    for (i <- 1 to 100) {
      hashResult = hashResult.+:(
        printExecTime(keyAccess(hashMap_100000)(SIZE_100000 / 2))(i)
      )
      treeResult = treeResult.+:(
        printExecTime(keyAccess(treeMap_100000)(SIZE_100000 / 2))(i)
      )
    }

    printAverage("HashMap", hashResult, "TreeMap", treeResult)

    println(s"対象件数: ${SIZE_100000}件 対象キー：最後に登録した要素のキー")

    hashResult = List.empty[Long]
    treeResult = List.empty[Long]

    for (i <- 1 to 100) {
      hashResult =
        hashResult.+:(printExecTime(keyAccess(hashMap_100000)(SIZE_100000))(i))
      treeResult =
        treeResult.+:(printExecTime(keyAccess(treeMap_100000)(SIZE_100000))(i))
    }

    printAverage("HashMap", hashResult, "TreeMap", treeResult)

    println(s"対象件数: ${SIZE_1000000}件 対象キー：最初に登録した要素のキー")

    hashResult = List.empty[Long]
    treeResult = List.empty[Long]

    for (i <- 1 to 100) {
      hashResult =
        hashResult.+:(printExecTime(keyAccess(hashMap_1000000)(1))(i))
      treeResult =
        treeResult.+:(printExecTime(keyAccess(treeMap_1000000)(1))(i))
    }

    printAverage("HashMap", hashResult, "TreeMap", treeResult)

    println(s"対象件数: ${SIZE_1000000}件 対象キー：中間に登録した要素のキー")

    hashResult = List.empty[Long]
    treeResult = List.empty[Long]

    for (i <- 1 to 100) {
      hashResult = hashResult.+:(
        printExecTime(keyAccess(hashMap_1000000)(SIZE_1000000 / 2))(i)
      )
      treeResult = treeResult.+:(
        printExecTime(keyAccess(treeMap_1000000)(SIZE_1000000 / 2))(i)
      )
    }

    printAverage("HashMap", hashResult, "TreeMap", treeResult)

    println(s"対象件数: ${SIZE_1000000}件 対象キー：最後に登録した要素のキー")

    hashResult = List.empty[Long]
    treeResult = List.empty[Long]

    for (i <- 1 to 100) {
      hashResult = hashResult.+:(
        printExecTime(keyAccess(hashMap_1000000)(SIZE_1000000))(i)
      )
      treeResult = treeResult.+:(
        printExecTime(keyAccess(treeMap_1000000)(SIZE_1000000))(i)
      )
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
      hashResult =
        hashResult.+:(printExecTime(sequentialAccess(hashMap_100000))(i))
      listResult =
        listResult.+:(printExecTime(sequentialAccess(listMap_100000))(i))
      treeResult =
        treeResult.+:(printExecTime(sequentialAccess(treeMap_100000))(i))
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
      hashResult =
        hashResult.+:(printExecTime(sequentialAccess(hashMap_500000))(i))
      listResult =
        listResult.+:(printExecTime(sequentialAccess(listMap_500000))(i))
      treeResult =
        treeResult.+:(printExecTime(sequentialAccess(treeMap_500000))(i))
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
      hashResult =
        hashResult.+:(printExecTime(sequentialAccess(hashMap_1000000))(i))
      treeResult =
        treeResult.+:(printExecTime(sequentialAccess(treeMap_1000000))(i))
    }

    printAverage("HashMap", hashResult, "TreeMap", treeResult)

    println(s"対象件数: ${SIZE_10000000}件")

    hashResult = List.empty[Long]
    treeResult = List.empty[Long]

    for (i <- 1 to 100) {
      hashResult =
        hashResult.+:(printExecTime(sequentialAccess(hashMap_10000000))(i))
      treeResult =
        treeResult.+:(printExecTime(sequentialAccess(treeMap_10000000))(i))
    }

    printAverage("HashMap", hashResult, "TreeMap", treeResult)
  }

  def vectorMapListMap() = {

    println(s"データ追加 ListMap vs VectorMap")

    // 処理時間格納のリスト
    var listResult = List.empty[Long]
    var vectorResult = List.empty[Long]

    // 空コレクションデータ
    val eList = ListMap.empty[Int, Int]
    val eVector = VectorMap.empty[Int, Int]

    println(s"対象件数: ${SIZE_100000}件")

    listResult = List.empty[Long]
    vectorResult = List.empty[Long]

    for (i <- 1 to 100) {
      listResult =
        listResult.+:(printExecTime(addMapProc(eList)(SIZE_100000))(i))
      vectorResult =
        vectorResult.+:(printExecTime(addMapProc(eVector)(SIZE_100000))(i))
    }

    printAverage(
      "ListMap",
      listResult,
      "VectorMap",
      vectorResult
    )

    println(s"キーアクセス ListMap vs VectorMap")

    // アクセス性能検証用データ
    val listMap_100000 = addMapProc(eList)(SIZE_100000)
    val vectorMap_100000 = addMapProc(eVector)(SIZE_100000)

    println(s"対象件数: ${SIZE_100000}件 対象キー：最初に登録した要素のキー")

    listResult = List.empty[Long]
    vectorResult = List.empty[Long]

    for (i <- 1 to 100) {
      listResult = listResult.+:(printExecTime(keyAccess(listMap_100000)(1))(i))
      vectorResult = vectorResult.+:(printExecTime(keyAccess(vectorMap_100000)(1))(i))
    }

    printAverage(
      "ListMap",
      listResult,
      "VectorMap",
      vectorResult
    )

    println(s"対象件数: ${SIZE_100000}件 対象キー：中間に登録した要素のキー")

    listResult = List.empty[Long]
    vectorResult = List.empty[Long]

    for (i <- 1 to 100) {
      listResult = listResult.+:(printExecTime(keyAccess(listMap_100000)(SIZE_100000/2))(i))
      vectorResult = vectorResult.+:(printExecTime(keyAccess(vectorMap_100000)(SIZE_100000/2))(i))
    }

    printAverage(
      "ListMap",
      listResult,
      "VectorMap",
      vectorResult
    )

    println(s"対象件数: ${SIZE_100000}件 対象キー：最後に登録した要素のキー")

    listResult = List.empty[Long]
    vectorResult = List.empty[Long]

    for (i <- 1 to 100) {
      listResult = listResult.+:(printExecTime(keyAccess(listMap_100000)(SIZE_100000))(i))
      vectorResult = vectorResult.+:(printExecTime(keyAccess(vectorMap_100000)(SIZE_100000))(i))
    }

    printAverage(
      "ListMap",
      listResult,
      "VectorMap",
      vectorResult
    )

    println(s"シーケンシャルアクセス ListMap vs VectorMap")

    println(s"対象件数: ${SIZE_100000}件")

    for (i <- 1 to 100) {
      listResult =
        listResult.+:(printExecTime(sequentialAccess(listMap_100000))(i))
      vectorResult =
        vectorResult.+:(printExecTime(sequentialAccess(vectorMap_100000))(i))
    }

    printAverage(
      "ListMap",
      listResult,
      "VectorMap",
      vectorResult
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
    println(s"${p1}平均処理時間: ${p1Result.foldLeft(0L)(_ + _) / p1Result.size} ミリ秒")
    println(s"${p2}平均処理時間: ${p2Result.foldLeft(0L)(_ + _) / p2Result.size} ミリ秒")
  }

  def printAverage(p1: String,
                   p1Result: List[Long],
                   p2: String,
                   p2Result: List[Long],
                   p3: String,
                   p3Result: List[Long]): Unit = {
    println(s"${p1}平均処理時間: ${p1Result.foldLeft(0L)(_ + _) / p1Result.size} ミリ秒")
    println(s"${p2}平均処理時間: ${p2Result.foldLeft(0L)(_ + _) / p2Result.size} ミリ秒")
    println(s"${p3}平均処理時間: ${p3Result.foldLeft(0L)(_ + _) / p3Result.size} ミリ秒")
  }

  def randomInt(upper: Int): Int = {
    RANDOM.nextInt(upper)
  }
}
