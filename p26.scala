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

case class ApproximationState(denom: Int, mag: Int, approx: Int, error: Int)

object Hi {
  def main(args: Array[String]) =
    println(inverseRepeatLength(args.first.toInt))

  def maxRepeatsBelow(n: Int) =
    Iterator.range(2,n).map(inverseRepeatLength).max

  def inverseRepeatLength(x: Int) =
    approximationsFrom(ApproximationState(x,1,0,1)).takeWhile(s=>s.error>0)

  def minFun(f: Int=>Int) =
    (x: Int,y: Int) => if(f(y) < f(x)) y else x

  def decimalFractionError(denom: Int, magnitude: Int) : Int=>Int =
    c => math.abs(c * denom - magnitude)

  def betterDecimalFractionApproxFor(denom: Int, magnitude: Int) =
    minFun(decimalFractionError(denom, magnitude))

  def approximationsFrom(a: ApproximationState) : Stream[ApproximationState] = {
    val candidateApproxes = Iterator.range(-5,6).map((10 * a.approx).+)
    val nextMag = a.mag * 10
    val nextApprox = candidateApproxes.reduceLeft(betterDecimalFractionApproxFor(a.denom, nextMag))
    val nextState = ApproximationState(a.denom, nextMag, nextApprox, decimalFractionError(a.denom, nextMag)(nextApprox))
    a #:: approximationsFrom(nextState)
  }

}

// vim: set ts=2 sw=2 et:
