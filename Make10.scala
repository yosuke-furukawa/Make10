import scala.collection.mutable.Set
import scala.collection.mutable.HashSet
import scala.collection.mutable.ListBuffer

/**
 * 四則演算(+, -, *, /)を使って10を作るプログラム
 */
object Make10 {
  /**
   * メモ化用集合
   */
  private val answerSet = new HashSet[String]()
  
  private val ANSWER_NUM = 10

  /**
   * メモ化
   */
  def memorize(answer: String) {
    answerSet.add(answer);
  }

  /**
   * 四則演算で10を作るメソッドです。
   */
  def make10(nums: List[Int]) {
    val opList = Array("+", "-", "*", "/");
    val numList = new NumberList(nums).list
    val range = 0 to 3
    val ansList = new ListBuffer[Tuple2[Rational, String]]();
    for (nums <- numList) {
      for (op1 <- opList) {
        for (op2 <- opList) {
          for (op3 <- opList) {
            pattern1(nums._1, nums._2, nums._3, nums._4, op1, op2, op3).foreach { value => ansList += value}
            pattern2(nums._1, nums._2, nums._3, nums._4, op1, op2, op3).foreach { value => ansList += value}
            pattern3(nums._1, nums._2, nums._3, nums._4, op1, op2, op3).foreach { value => ansList += value}
            pattern4(nums._1, nums._2, nums._3, nums._4, op1, op2, op3).foreach { value => ansList += value}
            pattern5(nums._1, nums._2, nums._3, nums._4, op1, op2, op3).foreach { value => ansList += value}
          }
        }
      }
    }
    val filteredList = ansList.filter(_._1 == ANSWER_NUM)
    filteredList.foreach(ans => println(ans._2 + " = " + ans._1))
  }

  /**
   * ((a op1 b) op2 c) op3 d の計算。
   */
  def pattern1(
    first: Rational,
    second: Rational,
    third: Rational,
    fourth: Rational,
    op1: String,
    op2: String,
    op3: String): Option[(Rational, String)] = {
    val exp = "((" + first + op1 + second + ")" + op2 + third + ")" + op3 + fourth
    var result: Option[(Rational, String)] = None;
    if (!answerSet.contains(exp)) {
      memorize(exp)
      calculate(op1, first, second).foreach {
        ans1 =>
          calculate(op2, ans1, third).foreach {
            ans2 =>
              calculate(op3, ans2, fourth).foreach {
                ans3 => result = Some(ans3, exp)
              }
          }
      }
    }
    result
  }
  
  /**
   * (a op1 b) op2 (c op3 d) の計算。
   */
  def pattern2(
    first: Rational,
    second: Rational,
    third: Rational,
    fourth: Rational,
    op1: String,
    op2: String,
    op3: String): Option[(Rational, String)] = {
    val exp = "(" + first + op1 + second + ")" + op2 + "(" + third + op3 + fourth + ")"
    var result: Option[(Rational, String)] = None;
    if (!answerSet.contains(exp)) {
      memorize(exp)
      calculate(op1, first, second).foreach {
        ans1 =>
          calculate(op3, third, fourth).foreach {
            ans2 =>
              calculate(op2, ans1, ans2).foreach {
                ans3 => result = Some(ans3, exp)
              }
          }
      }
    }
    result
  }

  /**
   * (a op1 (b op2 c)) op3 d の計算。
   */
  def pattern3(
    first: Rational,
    second: Rational,
    third: Rational,
    fourth: Rational,
    op1: String,
    op2: String,
    op3: String): Option[(Rational, String)] = {
    val exp = "(" + first + op1 + "(" + second + op2 + third + "))" + op3 + fourth
    var result: Option[(Rational, String)] = None;
    if (!answerSet.contains(exp)) {
      memorize(exp)
      calculate(op2, second, third).foreach {
        ans1 =>
          calculate(op1, first, ans1).foreach {
            ans2 =>
              calculate(op3, ans2, fourth).foreach {
                ans3 => result = Some(ans3, exp)
              }
          }
      }
    }
    result
  }
  
  /**
   * a op1 ((b op2 c) op3 d) の計算。
   */
  def pattern4(
    first: Rational,
    second: Rational,
    third: Rational,
    fourth: Rational,
    op1: String,
    op2: String,
    op3: String): Option[(Rational, String)] = {
    val exp = first + op1 + "((" + second + op2 + third + ")" + op3 + fourth + ")"
    var result: Option[(Rational, String)] = None;
    if (!answerSet.contains(exp)) {
      memorize(exp)
      calculate(op2, second, third).foreach {
        ans1 =>
          calculate(op3, ans1, fourth).foreach {
            ans2 =>
              calculate(op1, first, ans2).foreach {
                ans3 => result = Some(ans3, exp)
              }
          }
      }
    }
    result
  }
  
  /**
   * a op1 (b op2 (c op3 d)) の計算。
   */
  def pattern5(
    first: Rational,
    second: Rational,
    third: Rational,
    fourth: Rational,
    op1: String,
    op2: String,
    op3: String): Option[(Rational, String)] = {
    val exp = first + op1 + "(" + second + op2 + "(" + third + op3 + fourth + "))"
    var result: Option[(Rational, String)] = None;
    if (!answerSet.contains(exp)) {
      memorize(exp)
      calculate(op3, third, fourth).foreach {
        ans1 =>
          calculate(op2, second, ans1).foreach {
            ans2 =>
              calculate(op1, first, ans2).foreach {
                ans3 => result = Some(ans3, exp)
              }
          }
      }
    }
    result
  }
  
  /**
   * 演算子と2つの数を受け取って値を返すメソッド。
   * 0除算や定義されていない演算子を使った場合、Noneが返る。
   */
  private def calculate(op: String, num1: Rational, num2: Rational): Option[Rational] = {
    try {
      val ans: Option[Rational] = op match {
        case "+" => Some(num1 + num2)
        case "-" => Some(num1 - num2)
        case "*" => Some(num1 * num2)
        case "/" => Some(num1 / num2)
        case _ => None
      }
      ans
    } catch {
      case _ => None
    }
  }

  def main(args: Array[String]) {
    make10(List(9, 9, 9, 9))
  }

}