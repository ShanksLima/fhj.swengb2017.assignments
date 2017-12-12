package at.fhj.swengb.apps.calculator

import java.util.NoSuchElementException
import scala.util.Try

/**
  * Companion object for our reverse polish notation calculator.
  */
object RpnCalculator {

  /**
    * Returns empty RpnCalculator if string is empty, otherwise pushes all operations
    * on the stack of the empty RpnCalculator.
    *
    * @param s a string representing a calculation, for example '1 2 +'
    * @return
    */
  def apply(s: String): Try[RpnCalculator] = {
    if (s.isEmpty)

      Try(RpnCalculator())
    else {   try {
        val stacking: List[Op] = s.split("\\s").map(x => Op(x)).toList
        stacking.foldLeft(Try(RpnCalculator())
        )((acc, element) => acc.get.push(element))
      }

    catch {
      case p: Exception => Try[RpnCalculator](throw p)
      }
    }
  }

}

/**
  * Reverse Polish Notation Calculator.
  *
  * @param stack a datastructure holding all operations
  */
case class RpnCalculator(stack: List[Op] = Nil) {

  /**
    * By pushing Op on the stack, the Op is potentially executed. If it is a Val, it the op instance is just put on the
    * stack, if not then the stack is examined and the correct operation is performed.
    *
    * @param op
    * @return
    */
  def push(op: Op): Try[RpnCalculator] = {

    op match {
      case one: Val => Try(RpnCalculator(stack :+ one))

      case two: BinOp =>
        try {
          def additonalValue(doMathe: RpnCalculator): Val = {
            val result = doMathe.peek()
            result match {
              case avalue: Val   => avalue
              case _: BinOp => throw new NoSuchElementException
            }
          }
          val first = additonalValue(this)
          var intermediateResult = pop()._2
          val second = additonalValue(intermediateResult)
          intermediateResult = intermediateResult.pop()._2
          val result: Val = two.eval(first, second)
          intermediateResult.push(result)
        } catch {
          case three: Exception => Try[RpnCalculator](throw three)
        }
    }
  }

  /**
    * Pushes val's on the stack.
    *
    * If op is not a val, pop two numbers from the stack and apply the operation.
    *
    * @param op
    * @return
    */
  def push(op: Seq[Op]): Try[RpnCalculator] = op.foldLeft(Try(RpnCalculator()))((acc, elem) => acc.get.push(elem))

  /**
    * Returns an tuple of Op and a RevPolCal instance with the remainder of the stack.
    *
    * @return
    */
  def pop(): (Op, RpnCalculator) = (stack.head, RpnCalculator(stack.tail))

  /**
    * If stack is nonempty, returns the top of the stack. If it is empty, this function throws a NoSuchElementException.
    *
    * @return
    */
  def peek(): Op = {
    if (stack.isEmpty)
      throw new NoSuchElementException
    else
      stack.head
  }

  /**
    * returns the size of the stack.
    *
    * @return
    */
  def size: Int = stack.size
}