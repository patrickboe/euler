def decRemainders(x: Int) = (Stream iterate 1){r => (r*10) % x}

def repeatLength(rems: Seq[Int], places: Map[Int,Int], place: Int) : Int = {
  val a = rems.head
  places get a match {
    case Some(i) => place - i
    case None => repeatLength(rems.tail, places + (a->place), place + 1)
  }
}

def inverseRepeatLength(x: Int) = repeatLength(decRemainders(x), Map(), 0)

def maxRepeatsBelow(n: Int) = (2 to n) maxBy inverseRepeatLength

println(maxRepeatsBelow(1000))

// vim: set ts=2 sw=2 et:
