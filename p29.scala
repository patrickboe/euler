/*
  1) find number of all pairs of a and b
  2) subtract from this the number of duplicates

  part one is the number of 2-permutations of the 99 numbers
  between 2 and 100: 99^2 = 9801

  part two, the number of duplicates, is the count of every
  power (c^i)^j with an integer k such that

  c^i <= 100
  k < i
  2 <= j <= 100
  2 <= ij/k <= 100
  ij % k == 0
  c >= 2

  (this is because both (c^k)^ij/k and (c^i)^j are
  counted in step 1 as separate items, even though they
  are equal when evaluated)
*/

99 * 99 - (((
  for (
    c <- 2 to 10;
    i <- 2 to 6 if math.pow(c,i)<=100;
    k <- 1 until i;
    j <- 2 to 100 * k / i if (i * j % k == 0)
  ) yield (math.pow(c,i) toInt,j)) toSet) size)

// vim: set ts=2 sw=2 et:
