package observatory

import com.sksamuel.scrimage.Image
import com.sksamuel.scrimage.nio.PngWriter
import java.time.LocalDate
import java.nio.file.Files
import java.nio.file.Paths

object Main extends App {

  val colorScale: Map[Temperature,Color] = Map(
    60d	-> Color(255,255,255),
    32d	-> Color(255,0,0),
    12d	-> Color(255,255,0),
    0d	-> Color(0,255,255),
    -15d	-> Color(0,0,255),
    -27d	-> Color(255,0,255),
    -50d	-> Color(33,0,107),
    -60d	-> Color(0,0,0)
  )

  // Choose which stage(s) you want to run
  stage3a()

  def stage2() = {

    val year = 2015

    val measurements: Iterable[(LocalDate, Location, Temperature)] =
      Extraction.locateTemperatures(year, "/stations.csv", s"/$year.csv")

    val averages: Iterable[(Location, Temperature)] = Extraction.locationYearlyAverageRecords(measurements)

    println("Averages: " + averages.size)

    val img: Image = Visualization.visualize(averages,colorScale)

    implicit val writer = PngWriter.NoCompression
    img.output(new java.io.File(s"target/map-$year.png"))

  }

  def stage3() = {

    val locationsTemperatures = List(
      (new Location(45.0, -90.0), -20.0)
      , (new Location(45.0, 90.0), -10.0)
      , (new Location(0.0, 0.0), 0.0)
      , (new Location(-45.0, -90.0), 0.0)
      , (new Location(-45.0, 90.0), 20.0)
    )

    val colorMap = List(
        (-10.0, Color(255, 255, 255))
      , (-20.0, Color(255, 255, 0))
      , (0.0, Color(255, 0, 0))
      , (10.0, Color(0, 255, 0))
      , (20.0, Color(0, 0, 255))
    )

    val img = Interaction.tile(locationsTemperatures, colorMap, Tile(0, 0, 2))
    implicit val writer = PngWriter.NoCompression
    img.output(new java.io.File(s"target/test.png"))
  }


  def stage3a() = {

    implicit val writer = PngWriter.NoCompression

    val yearlyData: Seq[(Year, Iterable[(Location, Temperature)])] = for (
      year <- 2014 to 2015;
      measurements = Extraction.locateTemperatures(year, "/stations.csv", s"/$year.csv");
      averages = Extraction.locationYearlyAverageRecords(measurements)
    ) yield {
      (year, averages)
    }

    yearlyData.foreach( d => println(d._1,d._2.size) )

    def makeThatPic(year: Year, tile: Tile, data: Iterable[(Location, Temperature)]) = {

      println("makeThatPic ========================================================")
      println(s"year: $year")
      println(s"data: ${data.size}")
      println(s"tile: $tile")

      // target/temperatures/<year>/<zoom>/<x>-<y>.png.
      val img: Image = Interaction.tile(data,colorScale,tile)
      val pathname = s"target/temperatures/$year/${tile.zoom}/${tile.x}-${tile.y}.png"
      println(s"pathname: $pathname")
      Files.createDirectories(Paths.get(pathname).getParent)
      img.output(new java.io.File(pathname))
    }

    Interaction.generateTiles(yearlyData,makeThatPic)
  }

}

