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

  case class ApproximationState(denom: Int, mag: Int, approx: Int, error: Int)

  case class TabulationState(approxes: Seq[ApproximationState])

  def main(args: Array[String]) =
    println(inverseRepeatLength(args.first.toInt))

  def maxRepeatsBelow(n: Int) =
    Iterator.range(2,n).map(inverseRepeatLength).max

  val emptyPlaces =
    Vector.fill[Vector[Int]](10)(Vector.fill[Int](10)(0))

  def inverseRepeatLength(x: Int) =
    tabulationsFrom(
      TabulationState(
        emptyPlaces,
        imperfectInverseApproximations(x)))

  def imperfectInverseApproximations(x: Int) =
    approximationsFrom(ApproximationState(x,1,0,1)).takeWhile(s=>s.error != 0)

  def decimalFractionError(denom: Int, magnitude: Int, approx: Int) : Int=>Int =
    approx * denom - magnitude

  def newPlaces(places: IndexedSeq[IndexedSeq[Int]], approxState: ApproximationState) {
    val lastDigit = approxState.approx % 10
    places.updated(
      lastDigit,
      places[lastDigit].updated(approxState.error,log(10,approxState.mag))
    )
  }

  def tabulationsFrom(cur: TabulationState) : Stream[TabulationState] = {
    cur #::
    if(cur.tail.isEmpty)
      nil
    else
      tabulationsFrom(TabulationState(cur.approxes.tail, newPlaces(cur.places, cur.approxes.head)))
  }

  def approximationsFrom(cur: ApproximationState) : Stream[ApproximationState] = {
    cur #::
    {
      val candidateApproxes = Iterator.range(-5,6).map((10 * cur.approx).+)
      val mag = cur.mag * 10
      val approx = candidateApproxes.minBy(c=>math.abs(decimalFractionError(cur.denom, mag,c)))
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
