import scala.math.{Pi, abs, sqrt, toRadians}

package object observatory {
  type Temperature = Double // °C, introduced in Week 1
  type Year = Int // Calendar year, introduced in Week 1

  val TileSize = 256
  val R = 6371
  val acos1: Double = acos(1)
  val acosMinus1: Double = acos(-1)
  val TrigonometryPrecisionCoefficient = 8D //should be power of 2
  val SinDeviations: Array[Double] = (0 to (180 * TrigonometryPrecisionCoefficient).toInt)
    .map(degree => math.sin(toRadians(degree / TrigonometryPrecisionCoefficient)) - approxSin(degree / TrigonometryPrecisionCoefficient))
    .toArray
  val CosDeviations: Array[Double] = (0 to (180 * TrigonometryPrecisionCoefficient).toInt)
    .map(degree => math.cos(toRadians(degree / TrigonometryPrecisionCoefficient - 90)) - approxCos(degree / TrigonometryPrecisionCoefficient - 90))
    .toArray

  def measureTime[T](description: String)(func: => T): T = {
    println(s"Starting $description...")
    val startTime = System.currentTimeMillis()
    val res = func
    println(s"Finished $description... Took ${(System.currentTimeMillis() - startTime) / 1000} sec")
    res
  }

  def dividePair(pair: (Double, Int)) = pair._1 / pair._2

  def dividePairDouble(pair: (Double, Double)) = pair._1 / pair._2

  def approxSin(x: Double): Double = 4 * x * (180 - x) / (40500 - x * (180 - x))

  def approxCos(x: Double): Double = -5 * x * x / (32400 + x * x) + 1

  def sin(x: Double): Double = {
    if (x >= 0 && x <= 180) {
      val res = approxSin(x) + SinDeviations((x * TrigonometryPrecisionCoefficient).round.toInt)
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
      val res = approxCos(x) + CosDeviations(((x + 90) * TrigonometryPrecisionCoefficient).round.toInt)
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
      Pi - ret
    } else {
      ret
    }
  }
}
