package observatory

import com.sksamuel.scrimage.{Image}
import scala.math.pow

/**
  * 3rd milestone: interactive visualization
  */
object Interaction extends InteractionInterface {

  /**
    * @author acdhirr
    * @param tile Tile coordinates
    * @return The latitude and longitude of the top-left corner of the tile, as per http://wiki.openstreetmap.org/wiki/Slippy_map_tilenames
    */
  def tileLocation(tile: Tile): Location = tile.toLocation

  /**
    * @author acdhirr
    * @param temperatures Known temperatures
    * @param colors Color scale
    * @param tile Tile coordinates
    * @return A 256×256 image showing the contents of the given tile
    */
  def tile(temperatures: Iterable[(Location, Temperature)], colors: Iterable[(Temperature, Color)], tile: Tile): Image = {

    val rTemperatures = temperatures.map{ case(loc,temp) => (loc.asRadians,temp) }

    // offsets for this tile
    val (x0,y0) = tile.offSet

    val calculatedTemperatures: Seq[((Int,Int), Temperature)] = (
      for {
        x <- (y0 to y0 + 255).par;
        y <- (x0 to x0 + 255).par;
        // Hint: you will have to compute the corresponding latitude and longitude of each pixel within a tile.
        // A simple way to achieve that is to rely on the fact that each pixel in a tile can be thought of
        // as a sub tile at a higher zoom level (256 = 2⁸).
        location = Tile(x, y, tile.zoom + 8).toLocation
      } yield {
        val pt = Visualization.predictTemperature(rTemperatures,location.asRadians)
        ( (x-x0,y-y0), pt ) // remove offsets for 256x256 pic
      }
    ).seq

    Visualization.coloredTemperatureImage(calculatedTemperatures,colors,256, 127)
  }

  /**
    * Generates all the tiles for zoom levels 0 to 3 (included), for all the given years.
    * The dataset contains pairs of (Year, Data) values, or, said otherwise, data associated with years.
    * In your case, this data will be the result of Extraction.locationYearlyAverageRecords.
    * The second parameter of the generateTiles method is a function that takes a year, the coordinates
    * of the tile to generate, and the data associated with the year, and computes the tile and writes
    * it on your filesystem.
    *
    * @author acdhirr
    * @param yearlyData Sequence of (year, data), where `data` is some data associated with
    *                   `year`. The type of `data` can be anything.
    * @param generateImage Function that generates an image given a year, a zoom level, the x and
    *                      y coordinates of the tile and the data to build the image from
    */
  def generateTiles[Data](
    yearlyData: Iterable[(Year, Data)],
    generateImage: (Year, Tile, Data) => Unit
  ): Unit = {

    for (
      zoom <- 0 to 3;
      (year, data) <- yearlyData;
      tiles = pow(2,zoom).toInt-1;
      x <- 0 to tiles;
      y <- 0 to tiles;
      tile = Tile(x,y,zoom)
    ) yield generateImage(year, tile, data)

  }
}
