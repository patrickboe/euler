val ringWidths = 2 to 1000 by 2

val ringPerims = ringWidths map (_ * 4)

def tupleSum(t: Tuple2[Int,Int]) = t._1 + t._2

val northeast: Stream[Int] = 1 #:: northeast zip ringPerims map tupleSum

val southeast: Stream[Int] = 1 #:: northeast zip ringWidths map tupleSum

val diagSum = (northeast ++ southeast reduce (_ + _)) * 2 + 1

println(diagSum)
// vim: set ts=2 sw=2 et:
