package observatory

import com.sksamuel.scrimage.{Image, Pixel}

import scala.collection.immutable
import scala.math._
import scala.collection.parallel.immutable.ParSeq

/**
  * 2nd milestone: basic visualization
  */
object Visualization extends VisualizationInterface {

  /**
    * @author acdhirr
    * @param temperatures Known temperatures: pairs containing a location and the temperature at this location
    * @param location Location where to predict the temperature
    * @return The predicted temperature at `location`
    */
  def predictTemperature(temperatures: Iterable[(Location, Temperature)], location: Location): Temperature = {

    // Inverse distance weighting https://en.wikipedia.org/wiki/Inverse_distance_weighting
    val radLocation: RadianLocation = location.asRadians

    val result =
      temperatures.par.map{
        case(loc,temp) => (loc.asRadians, temp)
      }
      .aggregate(
        (0d, 0d)  // acc start value
      )(
        (acc: (Double, Double), y: (RadianLocation, Temperature)) => {
          val idw = inverseDistanceWeight(y._1,radLocation) // weight for location y._1
          (acc._1 + idw * y._2, acc._2 + idw) // add weighted temperature left, add weight right
        },
        (a: (Double, Double), b: (Double, Double)) => (a._1 + b._1, a._2 + b._2)  // combine accumulators
      )

    result._1 / result._2
  }


  /**
    * An overloaded version of predict temperature that uses locations already converted to radians,
    * so the conversions need not be repeated for each call of this method (the known temperature
    * list me be very long).
    *
    * @author acdhirr
    * @param temperatures Known temperatures: pairs containing a location (in radians) and
    *                     the temperature at this location
    * @param radLocation Location (in radians) where to predict the temperature
    * @return The predicted temperature at `location`
    */
  def predictTemperature(temperatures: Iterable[(RadianLocation, Temperature)], radLocation: RadianLocation): Temperature = {

    // Inverse distance weighting https://en.wikipedia.org/wiki/Inverse_distance_weighting
    val result =
      temperatures.par.aggregate(
          (0d, 0d)  // acc start value
        )(
          (acc: (Double, Double), y: (RadianLocation, Temperature)) => {
            val idw = inverseDistanceWeight(y._1,radLocation) // weight for location y._1
            (acc._1 + idw * y._2, acc._2 + idw) // add weighted temperature left, add weight right
          },
          (a: (Double, Double), b: (Double, Double)) => (a._1 + b._1, a._2 + b._2)  // combine accumulators
        )

    result._1 / result._2
  }


  /**
    * @author acdhirr
    * @param points Pairs containing a value and its associated color
    * @param value The value to interpolate
    * @return The color that corresponds to `value`, according to the color scale defined by `points`
    */
  def interpolateColor(points: Iterable[(Temperature, Color)], value: Temperature): Color = {

    // Note that the given points are not sorted in a particular order.
    val temperatures = points.toMap

    val lesserList = temperatures.keySet.filter(_ < value)
    val greaterList = temperatures.keySet.filter(_ > value)
    val lower = if (lesserList.isEmpty) temperatures.keySet.min else lesserList.max
    val upper = if (greaterList.isEmpty) temperatures.keySet.max else greaterList.min

    val lowerColor = temperatures(lower)
    val upperColor = temperatures(upper)

    val range = upper - lower
    // when upper and lower coincide, i.e. at the upper and lower boundaries,
    // make fraction 1 so take the boundary color
    val fraction = if(range > 0) (value-lower)/(upper-lower) else 1

    val r = lowerColor.red    + fraction * (upperColor.red    - lowerColor.red)
    val g = lowerColor.green  + fraction * (upperColor.green  - lowerColor.green)
    val b = lowerColor.blue   + fraction * (upperColor.blue   - lowerColor.blue)

    Color(r.round.toInt, g.round.toInt, b.round.toInt)
  }

  /**
    * @author acdhirr
    * @param temperatures Known temperatures
    * @param colors Color scale
    * @return A 360×180 image where each pixel shows the predicted temperature at its location
    */
  def visualize(temperatures: Iterable[(Location, Temperature)], colors: Iterable[(Temperature, Color)]): Image = {

    val rTemps = temperatures.map {
      case(loc,temp) => (loc.asRadians,temp)
    }

    // For all pixels in the image find the corresponding coordinate
    // and estimated temperature
    val pixelTemperatures: ParSeq[((Int,Int), Temperature)] = for {
      x <- (0 to 359).par;
      y <- (0 to 179).par;
      location = xy2loc(x,y)
    } yield ((x,y), predictTemperature(rTemps, location.asRadians)) // uses overloaded predictTemperature

    val xyColors: Seq[((Int,Int), Color)] = pixelTemperatures.map{
      case(loc,temp) => ( loc, interpolateColor(colors,temp) )
    }.seq

    val pixels: Array[Pixel] = new Array[Pixel](xyColors.size)

    xyColors.par.foreach {
      // N.B. Our Color type ≠ com.sksamuel.scrimage.Color,
      // So we cannot use our Color directly in the constructor!
      case((x,y),color: Color) => pixels(y * 360 + x) = Pixel(color.red,color.green,color.blue,255)
    }

    Image(360,180,pixels)
  }

  /**
    * @author acdhirr
    * @param location
    * @return
    */
  def xy2loc(x:Int,y:Int): Location  = {

    Location(90-y, x-180)

    /*
      (0,0) = {90,-180}
      +-----------------------+
      |                       |
      |    (180,90) = {0,0}   |
      |           .           |
      |                       |
      |                       |
      +-----------------------+
                   (359,179) = {-89,179}
    */
  }


  /**
    * @author acdhirr
    * @param loc1
    * @param loc2
    * @param power
    * @return Inverse Distance Weight for two locations
    */
  def inverseDistanceWeight(loc1: RadianLocation, loc2: RadianLocation): Double = {

    val POWER = 4

    val dist = greatCircleDistance(loc1, loc2)
    1 / pow(if (dist < 1) 1 else dist, POWER)
  }


  /**
    * Calculate the spherical distance in km between 2 points on the globe
    * https://en.wikipedia.org/wiki/Great-circle_distance
    *
    * @author acdhirr
    * @param loc1
    * @param loc2
    * @return atcf distance between two locations
    */
  def greatCircleDistance(loc1: RadianLocation, loc2: RadianLocation): Double = {

    val r =  6371  // earth radius (average) in meters  6371.0088

    val deltaSigma =
      if (loc1.equals(loc2))          0
      else if (loc1.isAntipode(loc2)) Pi
      else
        acos( sin(loc1.lat) * sin(loc2.lat) + cos(loc1.lat) * cos(loc2.lat) * cos(abs(loc1.lon-loc2.lon)) )

    r * deltaSigma
  }

}

