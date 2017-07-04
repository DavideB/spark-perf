package spark.perf

import java.util.Random

import org.apache.commons.math3.distribution.ZipfDistribution

import scala.util.{Failure, Success, Try}

/**
  * Created by giovanniquattrocchi on 26/06/17.
  */

trait Logging {
  lazy val logger = LoggerFactory.getLogger(getClass)

  implicit def logging2Logger(anything: Logging): Logger = anything.logger
}

class ZipfRandom(val size: Int, val skew: Int, val seed: Int) extends Logging {

  val rnd = new Random(seed)
//  val harmonic: Double = (1 to size).foldLeft(0d)((a, b) => a + (1.0d / Math.pow(b, skew)))

  val zipfDistribution : Try[ZipfDistribution] = Try(new ZipfDistribution(size, skew))

  def nextInt() : Int = {
    zipfDistribution match {
      case Success(zipf) => zipf.sample()
      case Failure(_) => rnd.nextInt(size) + 1
    }
  }

  def log(String s) = {
    logger.info(s)
  }

//  def getProbability(rank: Int): Double = {
//    (1.0d / Math.pow (rank, skew) ) / harmonic
//  }

}