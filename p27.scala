import collection.mutable.BitSet

case class SieveUpdater(factor: Int, updateWith: Int=>Unit)

val isPrime: Int=>Boolean = {
  var definedTo = 2
  val sieve = BitSet(2)
  val sieveUpdaters = {
    def followingUpdates(x: Int) : Stream[SieveUpdater] = {
      var p = (Iterator from x) filter sieve next
      var m = p * p
      SieveUpdater(p, n =>
        if(m<=n){
          val mults = m to n by p
          sieve --= mults
          m = (mults last) + p
        }) #:: followingUpdates(p + 1)
    }
    followingUpdates(2)
  }

  def growTo(x: Int) = {
    sieve ++= (definedTo + 1) to x
    definedTo = x
    (sieveUpdaters
      takeWhile (s=>math.pow(s.factor,2) <= x)
      foreach (s=>s.updateWith(x)))
  }

  x => {
    if (x > definedTo) growTo(x)
    sieve(x)
  }
}

def quadPrimeRun(a: Int,b: Int) =
  ((Iterator from 0)
    map (x=> x * (a + x) + b)
    takeWhile isPrime)

def maxQuadraticPrimeRun(n: Int) = {
  (for (a <- n to -n if a % 2 != 1;
        b <- n to 2 if isPrime(b)) yield {
    (a,b)
  }) maxBy (t => quadPrimeRun(t._1,t._2).length)
}

println(maxQuadraticPrimeRun(1000))
// vim: set ts=2 sw=2 et:
