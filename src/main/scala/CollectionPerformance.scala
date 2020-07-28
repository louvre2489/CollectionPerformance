import scala.collection.immutable._
import scala.util.Random

object CollectionPerformance extends App with SeqPerformance with MapPerformance with SetPerformance {

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

//  vectorMapListMap()

  println("----------------------------------------")

//  treeMapTreeSeqMap()

  println("----------------------------------------")

  // Set
//  setAdd()

  println("----------------------------------------")

//  setDataAccess()

  println("----------------------------------------")

  setSequentialAccess()

  println("----------------------------------------")

  hashSetBitSet()
}
