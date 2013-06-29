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

keep a map M of digits 0-9 to how many digits back
that digit last appeared, along with what the
error term on the approximation was at that time.

if M[d] has the same error term as the current error,
this is the beginning of a repeat. yield the distance
stored for M[d].
*/
object Main {

  case class ApproximationState(digit: Int, error: Int)

  def main(args: Array[String]) =
    println(maxRepeatsBelow(args.first.toInt))

  def maxRepeatsBelow(n: Int) =
    Iterator.range(2,n).maxBy(inverseRepeatLength)

  def inverseRepeatLength(x: Int) =
    repeatLength(
      imperfectInverseApproximations(x),
      Map.empty[Int,Int],
      0)

  def imperfectInverseApproximations(x: Int) =
    approximationsFrom(x, ApproximationState(0,1)).takeWhile(s=>s.error != 0)

  def decimalFractionError(denom: Int, target: Int, approx: Int) =
    target - approx * denom

  def repeatLength(
    approxes: Seq[ApproximationState],
    places: Map[Int,Int],
    place: Int) : Int = {
    if(approxes.tail.isEmpty)
      0
    else {
      val a = approxes.head
      val stateIndex = a.error * 20 + a.digit
      val occurrence = places.get(stateIndex)
      occurrence match {
        case Some(i) =>
           place - i
        case None =>
          repeatLength(
            approxes.tail,
            places.updated(stateIndex,place),
            place + 1)
      }
    }
  }

  def approximationsFrom(denom: Int, cur: ApproximationState)
      : Stream[ApproximationState] = {
    cur #::
    {
      val candidateApproxes = Iterator.range(-5,6)
      val target = cur.error * 10
      val approx = candidateApproxes.minBy(c =>
        math.abs(decimalFractionError(denom, target, c))
      )
      val nextState = ApproximationState(
        approx,
        decimalFractionError(denom, target, approx)
      )
      approximationsFrom(denom, nextState)
    }
  }

}

// vim: set ts=2 sw=2 et:
