package services

import scala.collection.mutable

/*
Service to perform calculation.
It converts expression to
reverse polish notation and
calculates it
 */
object CalculationService {
  @throws (classOf[IllegalArgumentException])
  def calculate(expression : String) : String = {
    val stack = mutable.Stack[Double]()
    var expressionRPN = ""
    //Conversion to reverse polish notation
    try {
      expressionRPN = convertToRPN(expression)
    }
    catch {
      case e : IllegalArgumentException => throw e

    }
    val values = expressionRPN.split(" ")
    //Calculating RPN-expression
    for (x <- values) {
      if (x forall Character.isDigit) {
        stack.push(x.toInt)
      }
      else {
        val b = stack.pop()
        val a = stack.pop()
        x match {
          case "+" => {
            stack.push(a + b)
          }
          case "-" => {
            stack.push(a - b)
          }
          case "*" => {
            stack.push(a * b)
          }
          case "/" => {
            stack.push(a / b)
          }
          case _ => throw new IllegalArgumentException ("Wrong calculation query")
        }
      }
    }
    //The last element of stack keeps calculated value
    return stack.pop().toString
  }
/*
Private method to convert
initial expression to
reverse polish notation
 */
  @throws (classOf[IllegalArgumentException])
  private def convertToRPN(origExp: String): String = {
    val sb = new StringBuilder()
    val stack = mutable.Stack[String]()
    var values = mutable.MutableList[String]()

    for (c <- origExp) {
      if (c.isDigit)
        sb.append(c)
      else {
        if (sb.nonEmpty) {
          values += sb.toString()
          sb clear()
        }
        //Ignoring spaces in expression
        if (c != ' ')
          values += c.toString
      }
    }
    if (sb.nonEmpty)
      values += sb.toString()
    sb clear()
    for (x <- values.toList) {
      //If is digit append to result string
      if (x forall Character.isDigit) {
        sb.append(x + " ")
      }
      //Adding operators to stack or result string
      //depending on its priority
      else {
        x match {
          case "(" => stack.push(x)
          case ")" if stack.nonEmpty => {
            while (stack.top != "(") {
              sb.append(stack.pop() + " ")
            }
            stack.pop()
          }
          case "+" | "-" => {
            while (stack.nonEmpty && stack.top != "(")
              sb.append(stack.pop() + " ")
            stack.push(x)
          }
          case "*" | "/" => {
            while (stack.nonEmpty && (stack.top == "*" || stack.top == "/"))
              sb.append(stack.pop() + " ")
            stack.push(x)
          }
          case _ => throw new IllegalArgumentException("Wrong calculation query")
        }
      }
    }
    while (stack.nonEmpty)
      sb.append(stack.pop() + " ")
    return sb.toString()
  }
}
