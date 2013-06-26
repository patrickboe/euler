/*
to find the decimal representation of 1/x,
from k = (-5 to 5) * (10 ^ -j) + i, multiply
k by x and choose the k with the smallest
difference from 1 to be the next i, and choose
j to be the increment of j. yield 0 when the
difference is zero.

take the tail of any tracked runs leading with the
second to last digit in the current decimal, and
discard any other tracked runs. increment c[t] for
each of the remaining tracked runs. For runs with an
empty tail, yield c[t]

examine whether the second to last digit in the
current decimal is at the head of any untracked runs.
if so, create approx track of that run with c[t] set to 0.

add the second to last digit to the end of every
untracked run, and add approx new run starting
with that digit
*/

import scala.math._

object Hi {
  def main(args: Array[String]) =
    println(inverseRepeatLength(args.first.toInt))

  def maxRepeatsBelow(n: Int) =
    Iterator.range(2,n).map(inverseRepeatLength).max

  def inverseRepeatLength(x: Int) =
    inverseRepeatLengthRecur(x, 10, 5)

  def minFun(f: Int=>Int) =
    (x: Int,y: Int) => if(f(y) < f(x)) y else x

  def decimalFractionError(denom: Int, magnitude: Int) : Int=>Int =
    c => abs(c * denom - magnitude)

  def betterDecimalFractionApproxFor(denom: Int, magnitude: Int) =
    minFun(decimalFractionError(denom, magnitude))

  def inverseRepeatLengthRecur(x: Int, mag: Int, approx: Int) : Int = {
    val candidateApproxes = Iterator.range(-5,6).map(approx.+)
    val bestApprox = candidateApproxes.reduceLeft(betterDecimalFractionApproxFor(x, mag))
    if(x * bestApprox == mag)
      bestApprox
    else
      inverseRepeatLengthRecur(x, mag*10, bestApprox*10)
  }
}

// vim: set ts=2 sw=2 et:
