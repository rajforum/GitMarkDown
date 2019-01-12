import java.util.Scanner
import scala.collection.mutable.Stack

class Demo {

  def calculate(operator : Char, operand1: String, operand2: String): String = {
    var result: Int = 0

    var op1 = operand1.toInt
    var op2 = operand2.toInt

    operator match {
      case '+' => result = op1 + op2
      case '-' => result = op1 - op2
      case '*' => result = op1 * op2
      case '/' => result = op2 / op1
    }
      result+""
  }

  def getDigit(x: Char): Int = {
    x match {
      case '0' => 0
      case '1' => 1
      case '2' => 2
      case '3' => 3
      case '4' => 4
      case '5' => 5
      case '6' => 6
      case '7' => 7
      case '8' => 8
      case '9' => 9
    }

  }

  def getExpression(exp: String): String= {
    var index: Int = 0;
    var result: String="";

    for (index <- 0 until exp.length()) {
    var x = exp.charAt(index)
      if(x.equals(' ')) {}else {
        result += exp.charAt(index)
      }
    }
     result
  }

//  def doProcess(x:Char,num: Int, ): Unit ={
//
//  }

}

object Calculator {

  def main(args: Array[String]): Unit = {
    var sc = scala.io.StdIn

    val test = new Demo()

    var x: Char = ' '
    var num: String = ""
    var count: Int = 0

    var operator = Array('+', '-', '*', '/')
    var operand = Array('0', '1', '2', '3', '4', '5', '6', '7', '8', '9')

    var operatorStack = Stack[Char]()
    var operandStack = Stack[String]()
    var tempOperatorStack = Stack[Char]()


    var str = "3* 27  /9*2"
    str = test.getExpression(str)

    println("Expr: "+str )

    for (i <- 0 until str.length()) {
      var x = str.charAt(i)


      if (operand contains x) {
        num += x;

//        if (i == str.length - 1) {
//          operandStack.push(num);
//          num = ""
//          println(i + ": " + operandStack)
//
//        }
        println(i + ": " + operandStack)
      } else if ((operator contains x )|| (i == str.length - 1) ) {
        println("in: "+i+": "+operandStack)

        if (tempOperatorStack.length == 1) {
          operandStack.push(num);
          num = ""
          println("temp: " + i + ": " + operandStack)

          var sign = tempOperatorStack.pop();
          var op1 = operandStack.pop();
          var op2 = operandStack.pop()

          var res = test.calculate(sign, op1, op2)
          println("Result: " + res)

          operandStack.push(res)
          println("res: " + i + ": " + operandStack)
          operatorStack.push(x)
        } else {

          if (x.equals('*') || x.equals('/')) {
            operandStack.push(num);
            num = ""
            tempOperatorStack.push(x)
            println("* /: " + i + ": " + operandStack)
          } else {
            operandStack.push(num);
            num = ""
            operatorStack.push(x)
            println("+: " + i + ": " + operandStack)
          }
        }
        println(s"elseif:$i $operandStack")
      }
      println("---------------------")
    }

  }
}

