import scala.annotation.tailrec

trait distributions {

  // Compute factorial of a given number
  def permutations(n: Int): Int = {
    @tailrec
    def permutationsAcc(n: Int, acc: Int): Int = n match {
      case 0 | 1 => acc
      case _ => permutationsAcc(n-1, n * acc)
    }
    permutationsAcc(n, 1)
  }
  // Compute number of ways to choose x from n
  def combinations(n: Int, x:Int): Int = {
    permutations(n) / (permutations(x) * permutations(n - x))
  }
}


class binomial(val trials: Int, val prob_success: Double) extends distributions {

  // get the probability for a specific value
  def getProb(value: Int): Double = {
    combinations(trials, value) * (Math.pow(prob_success, value) * Math.pow(1 - prob_success, trials - value))
  }
  // get the probability distribution for all possible values
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

