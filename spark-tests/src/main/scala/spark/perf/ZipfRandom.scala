package spark.perf

import java.util.Random
import org.slf4j._

/**
  * Created by giovanniquattrocchi on 26/06/17.
  */
trait Logging {
  lazy val logger = LoggerFactory.getLogger(getClass)

  implicit def logging2Logger(anything: Logging): Logger = anything.logger
}

class ZipfRandom(val size: Int, val skew: Int, val seed: Int) extends Logging{
  logger.info("parameters = "+size + " " + skew + " " + seed)
  val rnd = new Random(seed)
  val harmonic: Double = (1 to size).foldLeft(0d)((a, b) => a + (1.0d / Math.pow(b, skew)))

  def nextInt() : Int = {

    var rank: Int = 0
    var p, dice: Double = 0

    do {
      rank = rnd.nextInt(size) + 1
      p = getProbability(rank)
      dice = rnd.nextDouble()
      logger.info("dice "+dice+" p "+p)
    }
    while (dice >= p)
    logger.info("returning rank "+rank)
    rank
  }


  def getProbability(rank: Int): Double = {
    (1.0d / Math.pow (rank, skew) ) / harmonic
  }

}