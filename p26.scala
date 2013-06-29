def decRemainders(x: Int) = (Stream iterate 1){r => (r*10) % x}

def period(x: Int) = {
  def p(rems: Seq[Int], places: Map[Int,Int], place: Int) : Int = {
    val a = rems.head
    places get a match {
      case Some(i) => place - i
      case None => p(rems.tail, places + (a->place), place + 1)
    }
  }
  p(decRemainders(x), Map(), 0)
}

println((2 to 1000) maxBy period)

// vim: set ts=2 sw=2 et:
