package observatory

import observatory.Visualization.predictTemperature

/**
  * 4th milestone: value-added information
  */
object Manipulation extends ManipulationInterface {

  /**
    * @param temperatures Known temperatures
    * @return A function that, given a latitude in [-89, 90] and a longitude in [-180, 179],
    *         returns the predicted temperature at this location
    */
  def makeGrid(temperatures: Iterable[(Location, Temperature)]): GridLocation => Temperature = {
    gridFunction(
      Array.tabulate(360 * 180) { i =>
        predictTemperature(temperatures, location(i))
      }
    )
  }

  /**
    * @param temperaturess Sequence of known temperatures over the years (each element of the collection
    *                      is a collection of pairs of location and temperature)
    * @return A function that, given a latitude and a longitude, returns the average temperature at this location
    */
  def average(temperaturess: Iterable[Iterable[(Location, Temperature)]]): GridLocation => Temperature = {
    val grids = temperaturess
      .map(yearTemperatures => makeGrid(yearTemperatures))

    gridFunction(
      Array.tabulate(360 * 180) { i =>
        val gridLoc = gridLocation(i)
        dividePair(
          grids
            .foldLeft((0D, 0))((a, grid) => (a._1 + grid(gridLoc), a._2 + 1))
        )
      }
    )
  }

  /**
    * @param temperatures Known temperatures
    * @param normals      A grid containing the “normal” temperatures
    * @return A grid containing the deviations compared to the normal temperatures
    */
  def deviation(temperatures: Iterable[(Location, Temperature)], normals: GridLocation => Temperature): GridLocation => Temperature = {
    val temperaturesGrid = makeGrid(temperatures)
    gridFunction(
      Array.tabulate(360 * 180) { i =>
        val gridLoc = gridLocation(i)
        temperaturesGrid(gridLoc) - normals(gridLoc)
      }
    )
  }


  def gridFunction(gridArray: Array[Temperature]): GridLocation => Temperature = {
    gridLocation: GridLocation => {
      gridArray(gridIndex(gridLocation))
    }
  }

  def gridIndex(gridLocation: GridLocation): Int = (90 - gridLocation.lat) * 360 + gridLocation.lon + 180

  def gridLocation(gridIndex: Int): GridLocation = GridLocation(90 - gridIndex / 360, gridIndex % 360 - 180)

  def location(gridIndex: Int): Location = Location(90 - gridIndex / 360, gridIndex % 360 - 180)
}

