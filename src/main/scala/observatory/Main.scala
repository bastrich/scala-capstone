package observatory

import java.io.File

import observatory.Extraction.{locateTemperatures, locationYearlyAverageRecords}
import observatory.Interaction.generateTiles
import observatory.Interaction2.{DeviationsColorScale, TemperaturesColorScale}
import observatory.Manipulation.{average, deviation, makeGrid}
import observatory.Visualization2.visualizeGrid

object Main extends App {
  val YearlyAverageTemperatures = measureTime("aggregate stations data") {
    for (year <- (1975 to 2015).par)
      yield (year, locationYearlyAverageRecords(locateTemperatures(year, "/stations.csv", s"/$year.csv")))
  }
  val YearsNormals = 1975 to 1989
  val YearsToCompare = 1990 to 2015

  generateTemperatures()
  generateDeviations()

  def generateTemperatures(): Unit = {
    val grids = YearlyAverageTemperatures.map { case (year, temperatures) =>
      measureTime(s"calculating temperatures grid for year $year") {
        (year, makeGrid(temperatures))
      }
    }

    generateTiles[GridLocation => Temperature](grids.seq, { case (year, currentTile, grid) =>
      measureTime(s"generating temperature image for " +
        s"year=$year, Tile(x=${currentTile.x}, y=${currentTile.y}, zoom=${currentTile.zoom})") {

        val dir = s"target/${LayerName.Temperatures.id}/$year/${currentTile.zoom}"
        mkDirRecursively(dir)

        visualizeGrid(grid, TemperaturesColorScale, currentTile).output(dir + tileImageName(currentTile))
      }
    })
  }

  def generateDeviations(): Unit = {
    val temperatureNormals = measureTime("averaging normals") {
      average(
        YearlyAverageTemperatures
          .filter { case (year, _) => YearsNormals.contains(year) }
          .map(_._2)
          .seq
      )
    }

    val deviationsGrids = YearlyAverageTemperatures
      .filter { case (year, _) => YearsToCompare.contains(year) }
      .map { case (year, temperatures) =>
        measureTime(s"calculating deviations grid for year $year") {
          (year, deviation(temperatures, temperatureNormals))
        }
      }

    generateTiles[GridLocation => Temperature](deviationsGrids.seq, { case (year, currentTile, deviationGrid) =>
      measureTime(s"generating deviation image for " +
        s"year=$year, Tile(x=${currentTile.x}, y=${currentTile.y}, zoom=${currentTile.zoom})") {

        val dir = s"target/${LayerName.Deviations.id}//$year/${currentTile.zoom}"
        mkDirRecursively(dir)

        visualizeGrid(deviationGrid, DeviationsColorScale, currentTile).output(dir + tileImageName(currentTile))
      }
    })
  }

  private def mkDirRecursively(path: String): Unit = {
    val dir = new File(path)
    if (!dir.exists()) {
      dir.mkdirs()
    }
  }

  private def tileImageName(tile: Tile): String = s"/${tile.x}-${tile.y}.png"
}
