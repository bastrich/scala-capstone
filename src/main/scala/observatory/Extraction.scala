package observatory

import java.time.LocalDate

import scala.io.Source

/**
  * 1st milestone: data extraction
  */
object Extraction extends ExtractionInterface {
  /**
    * @param year             Year number
    * @param stationsFile     Path of the stations resource file to use (e.g. "/stations.csv")
    * @param temperaturesFile Path of the temperatures resource file to use (e.g. "/1975.csv")
    * @return A sequence containing triplets (date, location, temperature)
    */
  def locateTemperatures(year: Year, stationsFile: String, temperaturesFile: String): Iterable[(LocalDate, Location, Temperature)] = {
    val stationsSource = Source
      .fromInputStream(getClass.getResourceAsStream(stationsFile))

    val stations = stationsSource
      .getLines()
      .map(line => line.split(","))
      .filterNot(station => station.length != 4 || station(2).isEmpty || station(3).isEmpty)
      .map(station => ((toInt(station(0)), toInt(station(1))), Location(station(2).toDouble, station(3).toDouble)))
      .toMap

    stationsSource.close

    val temperaturesSource = Source
      .fromInputStream(getClass.getResourceAsStream(temperaturesFile))

    val res = temperaturesSource
      .getLines()
      .map(line => line.split(","))
      .filterNot(observation => observation.length != 5 ||
        observation(2).isEmpty ||
        observation(3).isEmpty ||
        observation(4).isEmpty
      )
      .filter {
        case Array(stn, wban, _, _, _) => stations.contains((toInt(stn), toInt(wban)))
      }
      .map {
        case Array(stn, wban, month, day, temperature) =>
          (LocalDate.of(year, month.toInt, day.toInt), stations((toInt(stn), toInt(wban))), (temperature.toDouble - 32) * 5 / 9)
      }
      .toList

    temperaturesSource.close()

    res
  }

  private def toInt(s: String): Int = if (s.isEmpty) 0 else s.toInt

  /**
    * @param records A sequence containing triplets (date, location, temperature)
    * @return A sequence containing, for each location, the average temperature over the year.
    */
  def locationYearlyAverageRecords(records: Iterable[(LocalDate, Location, Temperature)]): Iterable[(Location, Temperature)] = {
    records
      .groupBy(_._2)
      .map {
        case (location, temperatures) => (
          location,
          dividePair(temperatures.map(_._3).foldLeft((0D, 0)) {
            case ((sum, count), temperature) => (sum + temperature, count + 1)
          })
        )
      }
      .toList
  }

}
