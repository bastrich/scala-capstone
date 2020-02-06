

import scala.math.{abs, sqrt, toRadians, Pi}

package object observatory {
  type Temperature = Double // °C, introduced in Week 1
  type Year = Int // Calendar year, introduced in Week 1

  val TileSize = 256
  val R = 6371
  val acos1 = acos(1)
  val acosMinus1 = acos(-1)

  def measureTime[T](description: String)(func: => T): T = {
    println(s"Starting $description...")
    val startTime = System.currentTimeMillis()
    val res = func
    println(s"Finished $description... Took ${(System.currentTimeMillis() - startTime) / 1000} sec")
    res
  }

  def dividePair(pair: (Double, Int)) = pair._1 / pair._2

  def dividePairDouble(pair: (Double, Double)) = pair._1 / pair._2

  val trigonometryPrecisionCoefficient = 8D //should be power of 2

  private val sinDeviations = (0 to (180 * trigonometryPrecisionCoefficient).toInt)
    .map(degree => math.sin(toRadians(degree / trigonometryPrecisionCoefficient)) - approxSin(degree / trigonometryPrecisionCoefficient))
    .toArray
  private val cosDeviations = (0 to (180 * trigonometryPrecisionCoefficient).toInt)
    .map(degree => math.cos(toRadians(degree / trigonometryPrecisionCoefficient - 90)) - approxCos(degree / trigonometryPrecisionCoefficient - 90))
    .toArray

  def approxSin(x: Double): Double = 4 * x * (180 - x) / (40500 - x * (180 - x))

  def approxCos(x: Double): Double = -5 * x * x / (32400 + x * x) + 1

  def sin(x: Double): Double = {
    if (x >= 0 && x <= 180) {
      val res = 4 * x * (180 - x) / (40500 - x * (180 - x)) + sinDeviations((x * trigonometryPrecisionCoefficient).round.toInt)
      if (res > 1) {
        1
      } else if (res < -1) {
        -1
      } else {
        res
      }
    } else if (x >= -180 && x < 0) {
      -sin(-x)
    } else {
      throw new Exception(s"Not valid angle $x")
    }
  }

  def cos(x: Double): Double = {
    if (x >= -90 && x <= 90) {
      val res = -5 * x * x / (32400 + x * x) + 1 + cosDeviations(((x + 90) * trigonometryPrecisionCoefficient).round.toInt)
      if (res > 1) {
        1
      } else if (res < -1) {
        -1
      } else {
        res
      }
    } else if (x > 90) {
      if (x <= 180) {
        -sin(x - 90)
      } else if (x <= 270) {
        -cos(x - 180)
      } else if (x <= 360) {
        sin(x - 270)
      } else {
        throw new Exception(s"Not valid angle $x")
      }
    } else {
      throw new Exception(s"Not valid angle $x")
    }
  }

  def acos(x: Double): Double = {
    val xAbs = abs(x)
    val ret = (((-0.0187293 * xAbs + 0.0742610) * xAbs - 0.2121144) * xAbs + 1.5707288) * sqrt(1.0 - xAbs)
    if (x < 0) {
      math.Pi - ret
    } else {
      ret
    }
  }
}
