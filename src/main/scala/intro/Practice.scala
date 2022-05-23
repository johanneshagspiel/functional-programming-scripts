package intro

/**
  * This part has some exercises for you to practice with the recursive lists and functions.
  * For the exercises in this part you are _not_ allowed to use library functions,
  * you should implement everything yourself.
  * Use recursion to process lists, iteration is not allowed.
  *
  * This part is worth 5 points.
  */
object Practice {

    /** Q5 (2p)
      * Implement the function that returns the first n elements from the list.
      *
      * @param xs list to take items from.
      * @param n amount of items to take.
      * @return the first n items of xs.
      */
    def firstN(xs: List[Int], n: Int): List[Int] = xs match {
        case Nil => Nil
        case x::tail =>
            if (n > 0) x :: firstN(tail, n-1) else Nil
    }


    /** Q6 (3p)
      * Implement the function that returns the maximum value in the list.
      *
      * @param xs list to process.
      * @return the maximum value in the list.
      */
    def maxValue(xs: List[Int]): Int =  {
        maxValue2(xs, Int.MinValue)
    }

    def maxValue2(xs: List[Int], maxValue: Int): Int = xs match {
        case Nil => maxValue
        case x::tail =>
            if (x > maxValue) maxValue2(tail, x) else maxValue2(tail, maxValue)
    }

    def main(args: Array[String]): Unit = {
        val three = 1 :: 2 :: 3000 :: 4 :: 5 :: 6 :: 100 :: Nil
        print(maxValue(three))
    }
}
