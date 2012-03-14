import scala.collection.mutable.ListBuffer

/**
 * 4つの数を受け取り、全ての組み合わせの数を返すクラス
 */
class NumberList(list: List[Int]) {
  private val numList = new ListBuffer[Tuple4[Rational, Rational, Rational, Rational]]()
  private val nums: List[Int] = list.toList
  private val range = 0 to nums.length - 1
  for (firstIndex <- range) {
    val first = new Rational(nums(firstIndex))
    for (
      secondIndex <- range if firstIndex != secondIndex
    ) {
      val second = new Rational(nums(secondIndex))
      for (
        thirdIndex <- range if firstIndex != thirdIndex && secondIndex != thirdIndex
      ) {
        val third = new Rational(nums(thirdIndex))
        for (
          fourthIndex <- range if firstIndex != fourthIndex &&
            secondIndex != fourthIndex &&
            thirdIndex != fourthIndex
        ) {
          val fourth = new Rational(nums(fourthIndex))
          val numbers = (first, second, third, fourth)
          if (!numList.contains(numbers))
            numList += numbers
        }
      }
    }
  }
  
  def list(): List[Tuple4[Rational, Rational, Rational, Rational]] = numList.toList
}