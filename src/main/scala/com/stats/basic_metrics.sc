
/*
Compute the mean of a numeric sequence
 */
def mean(seq: Seq[Double]): Double = {
  seq.sum / seq.length
}

/*
Find the median of a numeric sequence
*/

def median(seq: Seq[Double]): Double = {
  val seqLength = seq.length
  val ssorted = seq.sortWith(_ < _)

  if ( seqLength % 2 == 0){
    // median is the mean of two values in the middle
    val conc = ssorted(seqLength/2)::ssorted(seqLength/2 + 1)::Nil
    mean(conc)
  }else
    ssorted(seqLength/2 + 1)
}