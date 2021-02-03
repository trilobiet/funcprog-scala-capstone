package observatory

/**
  * 6th (and last) milestone: user interface polishing
  */
object Interaction2 extends Interaction2Interface {

  private val availableYears = 2014 to 2015

  private val temperatureColors = Seq(
    60d   -> Color(255,255,255),
    32d   -> Color(255,0,0),
    12d   -> Color(255,255,0),
    0d    -> Color(0,255,255),
    -15d  -> Color(0,0,255),
    -27d  -> Color(255,0,255),
    -50d  -> Color(33,0,107),
    -60d  -> Color(0,0,0)
  )

  private val deviationColors = Seq(
    7d  -> Color(0, 0, 0),
    4d  -> Color(255, 0, 0),
    2d  -> Color(255, 255, 0),
    0d  -> Color(255, 255, 255),
    -2d -> Color(0, 255, 255),
    -7d -> Color(0, 0, 255)
  )

  /**
    * @return The available layers of the application
    */
  def availableLayers: Seq[Layer] = {
    val yearsRange = Range(availableYears.min,availableYears.max)

    Seq(
      Layer(LayerName.Temperatures,temperatureColors,yearsRange),
      Layer(LayerName.Deviations,deviationColors,yearsRange)
    )
  }

  /**
    * @param selectedLayer A signal carrying the layer selected by the user
    * @return A signal containing the year bounds corresponding to the selected layer
    */
  def yearBounds(selectedLayer: Signal[Layer]): Signal[Range] = {
    Signal(selectedLayer().bounds)
  }

  /**
    * @param selectedLayer The selected layer
    * @param sliderValue The value of the year slider
    * @return The value of the selected year, so that it never goes out of the layer bounds.
    *         If the value of `sliderValue` is out of the `selectedLayer` bounds,
    *         this method should return the closest value that is included
    *         in the `selectedLayer` bounds.
    */
  def yearSelection(selectedLayer: Signal[Layer], sliderValue: Signal[Year]): Signal[Year] = {

    val bounds = yearBounds(selectedLayer)
    val year = sliderValue().max(bounds().min).min(bounds().max)
    Signal(year)
  }

  /**
    * @param selectedLayer The selected layer
    * @param selectedYear The selected year
    * @return The URL pattern to retrieve tiles
    */
  def layerUrlPattern(selectedLayer: Signal[Layer], selectedYear: Signal[Year]): Signal[String] = {

    val layerName = Signal(selectedLayer().layerName.id)
    Signal(s"target/${layerName()}/${selectedYear()}/{z}/{x}-{y}.png")
  }

  /**
    * @param selectedLayer The selected layer
    * @param selectedYear The selected year
    * @return The caption to show
    */
  def caption(selectedLayer: Signal[Layer], selectedYear: Signal[Year]): Signal[String] = {
    val dirName = Signal(selectedLayer().layerName.id.capitalize)
    Signal(s"${dirName()} (${selectedYear()})")
  }

}

// Interface used by the grading infrastructure. Do not change signatures
// or your submission will fail with a NoSuchMethodError.
trait Interaction2Interface {
  def availableLayers: Seq[Layer]
  def yearBounds(selectedLayer: Signal[Layer]): Signal[Range]
  def yearSelection(selectedLayer: Signal[Layer], sliderValue: Signal[Year]): Signal[Year]
  def layerUrlPattern(selectedLayer: Signal[Layer], selectedYear: Signal[Year]): Signal[String]
  def caption(selectedLayer: Signal[Layer], selectedYear: Signal[Year]): Signal[String]
}

sealed abstract class LayerName(val id: String)
object LayerName {
  case object Temperatures extends LayerName("temperatures")
  case object Deviations extends LayerName("deviations")
}

/**
  * @param layerName Name of the layer
  * @param colorScale Color scale used by the layer
  * @param bounds Minimum and maximum year supported by the layer
  */
case class Layer(layerName: LayerName, colorScale: Seq[(Temperature, Color)], bounds: Range)

