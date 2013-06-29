object Main {

  case class ApproximationState(digit: Int, error: Int)

  def main(args: Array[String]) =
    println(maxRepeatsBelow(args.head.toInt))

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
      places.get(stateIndex) match {
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
