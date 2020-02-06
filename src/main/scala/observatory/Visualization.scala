package observatory

import com.sksamuel.scrimage.Image

/**
  * 2nd milestone: basic visualization
  */
object Visualization extends VisualizationInterface {

  type TemperatureBound = (Temperature, Color)
  type TemperatureBounds = (TemperatureBound, TemperatureBound)

  /**
    * @param temperatures Known temperatures: pairs containing a location and the temperature at this location
    * @param location     Location where to predict the temperature
    * @return The predicted temperature at `location`
    */
  def predictTemperature(temperatures: Iterable[(Location, Temperature)], location: Location): Temperature =
    dividePairDouble(
      temperatures
        .foldLeft((0D, 0D)) { case (acc, (l, t: Temperature)) =>
          if (l.equals(location)) {
            return t
          }
          val distance = location.distance(l)
          if (distance < 1) {
            return t
          }
          (
            acc._1 + 1 / (distance * distance * distance * distance) * t,
            acc._2 + 1 / (distance * distance * distance * distance)
          )
        }
    )


  /**
    * @param points Pairs containing a value and its associated color
    * @param value  The value to interpolate
    * @return The color that corresponds to `value`, according to the color scale defined by `points`
    */
  def interpolateColor(points: Iterable[(Temperature, Color)], value: Temperature): Color =
    interpolateColor(
      points
        .foldLeft((points.head, points.head)) { case (bounds, possibleBound) =>
          if (value == possibleBound._1) {
            return possibleBound._2
          }
          updateUpperBound(updateLowerBound(bounds, possibleBound, value), possibleBound, value)
        },
      value
    )

  def interpolateColor(bounds: TemperatureBounds, value: Temperature): Color =
    if (value < bounds._1._1) {
      bounds._1._2
    } else if (value > bounds._2._1) {
      bounds._2._2
    } else {
      linearInterpolation(value, bounds._1._1, bounds._2._1, bounds._1._2, bounds._2._2)
    }

  def updateUpperBound(actualBounds: TemperatureBounds, possibleBound: TemperatureBound, value: Temperature): TemperatureBounds =
    if (actualBounds._2._1 < possibleBound._1 && actualBounds._2._1 < value
      || value < possibleBound._1 && possibleBound._1 < actualBounds._2._1) {
      (actualBounds._1, possibleBound)
    } else {
      actualBounds
    }

  def updateLowerBound(actualBounds: TemperatureBounds, possibleBound: TemperatureBound, value: Temperature): TemperatureBounds = {
    if (value < possibleBound._1 && possibleBound._1 < actualBounds._1._1
      || actualBounds._1._1 < possibleBound._1 && possibleBound._1 < value
      || possibleBound._1 < value && value < actualBounds._1._1) {
      (possibleBound, actualBounds._2)
    } else {
      actualBounds
    }
  }

  def linearInterpolation(x: Double,
                          x1: Double,
                          x2: Double,
                          y1: Color,
                          y2: Color): Color = Color(
    linearInterpolation(x, x1, x2, y1.red, y2.red).round.toInt,
    linearInterpolation(x, x1, x2, y1.green, y2.green).round.toInt,
    linearInterpolation(x, x1, x2, y1.blue, y2.blue).round.toInt
  )

  def linearInterpolation(x: Double,
                          x1: Double,
                          x2: Double,
                          y1: Double,
                          y2: Double): Double = (y1 * (x2 - x) + y2 * (x - x1)) / (x2 - x1)

  /**
    * @param temperatures Known temperatures
    * @param colors       Color scale
    * @return A 360×180 image where each pixel shows the predicted temperature at its location
    */
  def visualize(temperatures: Iterable[(Location, Temperature)], colors: Iterable[(Temperature, Color)]): Image =
    Image(
      360,
      180,
      Array.tabulate(360 * 180) { i =>
        interpolateColor(colors, predictTemperature(temperatures, indexToLocation(i))).pixel()
      }
    )

  def indexToLocation(index: Int): Location = {
    val x = index % 360
    val y = index / 360
    val lat = 90 - y
    val lon = x - 180
    Location(lat, lon)
  }
}

