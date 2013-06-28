/*
to find the decimal representation of 1/x,
from k = (-5 to 5) * (10 ^ -j) + i, multiply
k by x and choose the k with the smallest
difference from 1 to be the next i, and choose
j to be the increment of j. yield 0 when the
difference is zero.

let d be the last digit in the current approximation
if the approximation is too low, and the
last digit - 1 if the approximation is too high.

enqueue d to every untracked run, and add a new
run starting with that digit

examine whether d is at the head of any untracked runs.
if so, create a track of that run with c[t] set to 0.

take the tail of any tracked runs leading with
d, and discard any other tracked runs. increment
c[t] for each of the remaining tracked runs. For
runs with an empty tail, yield c[t]
*/
object Main {

  case class ApproximationState(denom: Int, mag: Int, approx: Int, error: Int)

  def main(args: Array[String]) =
    println(inverseRepeatLength(args.first.toInt))

  def maxRepeatsBelow(n: Int) =
    Iterator.range(2,n).map(inverseRepeatLength).max

  def inverseRepeatLength(x: Int) =

  def imperfectInverseApproximations(x: Int) =
    approximationsFrom(ApproximationState(x,1,0,1)).takeWhile(s=>s.error>0)

  def minFun(f: Int=>Int) =
    (x: Int,y: Int) => if(f(y) < f(x)) y else x

  def decimalFractionError(denom: Int, magnitude: Int) : Int=>Int =
    c => math.abs(c * denom - magnitude)

  def betterDecimalFractionApproxFor(denom: Int, magnitude: Int) =
    minFun(decimalFractionError(denom, magnitude))

  def approximationsFrom(cur: ApproximationState) : Stream[ApproximationState] = {
    cur #::
    {
      val candidateApproxes = Iterator.range(-5,6).map((10 * cur.approx).+)
      val mag = cur.mag * 10
      val approx = candidateApproxes.reduceLeft(betterDecimalFractionApproxFor(cur.denom, mag))
      val nextState = ApproximationState(
        cur.denom,
        mag,
        approx,
        decimalFractionError(cur.denom, mag)(approx)
      )
      approximationsFrom(nextState)
    }
  }

}

// vim: set ts=2 sw=2 et:
