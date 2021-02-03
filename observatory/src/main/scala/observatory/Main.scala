package observatory

import com.sksamuel.scrimage.Image
import com.sksamuel.scrimage.nio.PngWriter

import java.time.LocalDate
import java.nio.file.Files
import java.nio.file.Paths
import scala.math.pow

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

    val img = Interaction.tile(locationsTemperatures, colorMap, Tile(0, 0, 0))
    implicit val writer = PngWriter.NoCompression
    img.output(new java.io.File(s"target/test.png"))
  }


  def stage3a() = {

    // Creating temperature tiles
    // Takes a long, long time...

    implicit val writer = PngWriter.NoCompression

    val yearlyData: Seq[(Year, Iterable[(Location, Temperature)])] = for (
      year <- 2012 to 2013;
      measurements = Extraction.locateTemperatures(year, "/stations.csv", s"/$year.csv");
      averages = Extraction.locationYearlyAverageRecords(measurements)
    ) yield {
      (year, averages)
    }

    yearlyData.foreach( d => println((d._1,d._2.size)) )

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

  def stage4() = {

    val temperaturess: Seq[Iterable[(Location, Temperature)]] = for {
      year <- 1976 to 1977
    } yield {
      val measurements: Iterable[(LocalDate, Location, Temperature)] =
        Extraction.locateTemperatures(1976, "/stations.csv", s"/$year.csv")
      Extraction.locationYearlyAverageRecords(measurements)
    }

    val avgs = Manipulation.average(temperaturess)

    /*
    for (
      lat <- -89 to 90;
      lon <- -180 to 179
    ) println(avgs(GridLocation(lat,lon)))
    */

    avgs
  }

  def stage5() = {

    implicit val writer = PngWriter.NoCompression

    val normals: (GridLocation => Temperature) = stage4()

    val year = 2015

    val measurements: Iterable[(LocalDate, Location, Temperature)] =
      Extraction.locateTemperatures(year, "/stations.csv", s"/$year.csv")

    val averages2015: Iterable[(Location, Temperature)] = Extraction.locationYearlyAverageRecords(measurements)

    println("averages: " + averages2015.toMap.keySet.size)

    val deviation: GridLocation => Temperature = Manipulation.deviation(averages2015,normals)

    val colorScale: Map[Temperature,Color] = Map(
      7d	-> Color(0,0,0),
      4d	-> Color(255,0,0),
      2d	-> Color(255,255,0),
      0d	-> Color(255,255,255),
      -2d	-> Color(0,255,255),
      -7d	-> Color(0,0,255)
    )

    val tile1 = Tile(0,0,0)
    val img1: Image = Visualization2.visualizeGrid(deviation,colorScale,tile1)
    img1.output(new java.io.File("target/s1.png"))

    val tile2 = Tile(1,1,2)
    val img2: Image = Visualization2.visualizeGrid(deviation,colorScale,tile2)
    img2.output(new java.io.File("target/s2.png"))

    val tile3 = Tile(3,3,2)
    val img3: Image = Visualization2.visualizeGrid(deviation,colorScale,tile3)
    img3.output(new java.io.File("target/s3.png"))

    val tile4 = Tile(0,0,2)
    val img4: Image = Visualization2.visualizeGrid(deviation,colorScale,tile4)
    img4.output(new java.io.File("target/s4.png"))

  }


  def stage5a() = {

    val loc1 = Location(2.3,4.5)
    println(Visualization2.interpolationSquare(loc1))

    val loc2 = Location(-2.3,4.5)
    println(Visualization2.interpolationSquare(loc2))

    val loc3 = Location(2.3,4.1)
    println(Visualization2.interpolationSquare(loc3))

    val loc4 = Location(2.3,-4.1)
    println(Visualization2.interpolationSquare(loc4))

  }


  def stage5b() = {

    // Creating deviation tiles
    // Much faster then temperatures (stage3a)

    val years = 2012 to 2013

    implicit val writer = PngWriter.NoCompression

    val colorScale: Map[Temperature,Color] = Map(
      7d	-> Color(0,0,0),
      4d	-> Color(255,0,0),
      2d	-> Color(255,255,0),
      0d	-> Color(255,255,255),
      -2d	-> Color(0,255,255),
      -7d	-> Color(0,0,255)
    )

    val normals: (GridLocation => Temperature) = stage4()

    val deviations = (for {
      year <- years
      measurements =  Extraction.locateTemperatures(year, "/stations.csv", s"/$year.csv")
      averages = Extraction.locationYearlyAverageRecords(measurements)
    } yield {
      year -> Manipulation.deviation(averages, normals)
    }).toMap

    for (
      year <- years;
      zoom <- 0 to 3;
      tiles = pow(2,zoom).toInt-1;
      x <- 0 to tiles;
      y <- 0 to tiles;
      tile = Tile(x,y,zoom);
      img = Visualization2.visualizeGrid(deviations(year),colorScale,tile);
      pathname = s"target/deviations/$year/${tile.zoom}/${tile.x}-${tile.y}.png"
    ) yield {
      println(s"pathname: $pathname")
      Files.createDirectories(Paths.get(pathname).getParent)
      img.output(new java.io.File(pathname))
    }
  }

}

