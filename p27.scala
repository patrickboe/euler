import collection.mutable.BitSet

case class SieveUpdater(factor: Int, updateWith: Int=>Unit)

val isPrime: Int=>Boolean = {
  var definedTo = 2
  val sieve = BitSet(definedTo)
  val sieveUpdaters = {
    def followingUpdates(p: Int) : Stream[SieveUpdater] = {
      if(sieve(p)) {
        var m = p * p
        SieveUpdater(p, (n: Int) => {
          val mults = m to n by p
          sieve --= mults
          m = mults last
        }) #:: followingUpdates(p + 1)
      } else
        followingUpdates(p + 1)
    }
    followingUpdates(2)
  }

  def growTo(x: Int) = {
    sieve ++= (definedTo + 1) to x
    definedTo = x
    (sieveUpdaters takeWhile ((su: SieveUpdater)=>math.pow(su.factor,2) <= x)).
      foreach ((su: SieveUpdater)=>su.updateWith(x))
  }

  (x: Int) => {
    if (x > definedTo) growTo(x)
    sieve(x)
  }
}

def quadPrimeRun(a: Int,b: Int) =
  ((Iterator from 0) map (x=> x*x + a*x + b) takeWhile isPrime)

def maxQuadraticPrimeRun(n: Int) = {
  (for(a <- -n to n if a % 2 != 1; b <- 2 to n if isPrime(b))
    yield (a,b)) maxBy ((a: Int,b: Int) => quadPrimeRun(a,b).length).tupled
}

println(maxQuadraticPrimeRun(1000))
// vim: set ts=2 sw=2 et:
