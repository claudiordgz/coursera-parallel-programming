package demo

import common.parallel

object Account {
  private var firstUid = 0L
  private def getUniqueId = {
    firstUid += 1
    firstUid
  }
}

class PNorm {
  def power(x: Int, p: Double): Int = math.exp(p * math.log(math.abs(x))).toInt

  def sumSegment(a: Array[Int], p: Double, s: Int, t: Int): Int = {
    var i = s; var sum: Int = 0
    while(i < t) {
      sum = sum + power(a(i), p)
      i = i + 1
    }
    sum
  }

  def pNorm(a: Array[Int], p: Double): Int = {
    power(sumSegment(a, p, 0, a.length), 1/p)
  }

  def pNormTwoPart(a: Array[Int], p: Double): Int = {
    val m = a.length / 2
    val (sum1, sum2) = (sumSegment(a, p, 0, m),
      sumSegment(a, p, m, a.length))
    power(sum1 + sum2, 1/p)
  }

  def pNormTwoPartParallel(a: Array[Int], p: Double): Int = {
    val m = a.length / 2
    val (sum1, sum2) = parallel(sumSegment(a, p, 0, m),
      sumSegment(a, p, m, a.length))
    power(sum1 + sum2, 1/p)
  }
}

class Account( var amount: Int = 0) {
  val uid = Account.getUniqueId
  private def lockAndTransfer(target: Account, n: Int): Unit = {
    this.synchronized {
      target.synchronized {
        this.amount -= n
        target.amount += n
      }
    }
  }

  def transfer(target: Account, n: Int) =
    if (this.uid < target.uid) this.lockAndTransfer(target, n)
    else target.lockAndTransfer(this, -n)
}


object Main extends App {

  def startThread(a: Account, b: Account, n: Int) = {
    val t = new Thread {
      override def run() {
        for(i <- 0 until n) {
          a.transfer(b, 1)
        }
      }
    }
    t.start()
    t
  }

  val a1 = new Account(500000)
  val a2 = new Account(700000)
  val t = startThread(a1, a2, 150000)
  val s = startThread(a2, a1, 150000)
  s.join()
  t.join()

}
