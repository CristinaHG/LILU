import scala.annotation.tailrec

trait distributions {
  def permutations(n: Int): Int = {
    @tailrec
    def permutationsAcc(n: Int, acc: Int): Int = n match {
      case 0 | 1 => acc
      case _ => permutationsAcc(n-1, n * acc)
    }
    permutationsAcc(n, 1)
  }

  def combinations(n: Int, x:Int): Int = {
    permutations(n) / (permutations(x) * permutations(n - x))
  }
}


class binomial(val trials: Int, val prob_success: Double) extends distributions {
  def getProb(value: Int): Double = {
    combinations(trials, value) * (Math.pow(prob_success, value) * Math.pow(1 - prob_success, trials - value))
  }

  def getProbabilityDistrib: List[(Int, Double)]= {
    val valseq = List.range(0, trials + 1)
    valseq.map( v => (v, getProb(v)))
  }

}

object binomial extends App {
 val mybin = new binomial(3, 0.30)
//  print(mybin.getProb(3))
  print(mybin.getProbabilityDistrib)
}

