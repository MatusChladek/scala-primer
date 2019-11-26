package week4

abstract class BooleanType {
  def ifThenElse[T](t: => T, e: => T): T

  def &&(x: => BooleanType): BooleanType = ifThenElse(x, false)

  def ||(x: => BooleanType): BooleanType = ifThenElse(true, x)

  def unary_! : BooleanType = ifThenElse(false, true)

  def ==(x: => BooleanType): BooleanType = ifThenElse(x, x.unary_!)

  def !=(x: => BooleanType): BooleanType = ifThenElse(x.unary_!, x)

  /**
   * Implementation of comparison
   */
  def < (x: BooleanType):BooleanType = ifThenElse(false, x)
}