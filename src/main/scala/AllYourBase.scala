object AllYourBase:
   def rebase(fromBase: Int, number: List[Int], toBase: Int): Option[List[Int]] =
      def asDec =
         (for c <- (for n <- number.zip(number.indices.reverse)
                   yield n._1 * math.pow(fromBase, n._2).toInt).sum.toString
         yield c.toString.toInt).toList

      if fromBase <= 1 || toBase <= 1 || number.exists(_ < 0) || number.exists(_ >= fromBase) then None
      else if number.isEmpty then Some(List(0))
      else Some(asDec)
