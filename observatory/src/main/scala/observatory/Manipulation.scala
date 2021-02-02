package observatory

/**
  * 4th milestone: value-added information
  */
object Manipulation extends ManipulationInterface {

  /**
    * @author acdhirr
    * @param temperatures Known temperatures
    * @return A function that, given a latitude in [-89, 90] and a longitude in [-180, 179],
    *         returns the predicted temperature at this location
    */
  def makeGrid(temperatures: Iterable[(Location, Temperature)]): GridLocation => Temperature = {

    val rTemperatures = temperatures.map { case(loc,temp) => (loc.asRadians,temp) }
    def f(loc:GridLocation):Temperature = Visualization.predictTemperature(rTemperatures, loc.asRadians)
    val gridTemperatures = applyOnGrid(f)
    val grid = TemperatureGrid(gridTemperatures)

    // return getter function
    grid.get
  }

  /**
    * @author acdhirr
    * @param temperaturess Sequence of known temperatures over the years (each element of the collection
    *                      is a collection of pairs of location and temperature)
    * @return A function that, given a latitude and a longitude, returns the average temperature at this location
    */
  def average(temperaturess: Iterable[Iterable[(Location, Temperature)]]): GridLocation => Temperature = {

    val years = temperaturess.size
    val gridGetters: Iterable[GridLocation => Temperature] = temperaturess.map(yearData => makeGrid(yearData))

    // a closure for gridGetters
    def f(loc:GridLocation):Temperature = {
      // sum all temperatures at this grid location through the years and divide by number of years
      val tSum = gridGetters.par.foldLeft(0.0)((t: Temperature, f: GridLocation => Temperature) => t + f(loc))
      val tAvg = tSum / years
      tAvg
    }

    val avgTemperatures: Map[GridLocation, Temperature] = applyOnGrid(f)
    val grid = TemperatureGrid(avgTemperatures)

    // return getter function
    grid.get
  }

  /**
    * @author acdhirr
    * @param temperatures Known temperatures
    * @param normals A grid containing the “normal” temperatures
    * @return A grid containing the deviations compared to the normal temperatures
    */
  def deviation(temperatures: Iterable[(Location, Temperature)], normals: GridLocation => Temperature): GridLocation => Temperature = {

    val gridTemperatures = makeGrid(temperatures)
    def f(loc:GridLocation):Temperature = gridTemperatures(loc) - normals(loc)
    val deviantTemperatures: Map[GridLocation, Temperature] = applyOnGrid(f)
    val grid = TemperatureGrid(deviantTemperatures)

    // return getter function
    grid.get
  }

  /**
    * Apply a function for every coordinate in an Integer Grid
    * (-89 ≤ lat ≤ 90 , -180 ≤ lon ≤ 179)
    * (Abstracts away some boilerplate code)
    *
    * @author acdhirr
    * @param function a function that maps a temperature to a grid location
    * @return a Map mapping a temperature to each grid location
    */
  private def applyOnGrid(function:GridLocation=>Temperature): Map[GridLocation, Temperature] = {

    (
      for (
        lon <- (-180 to 179).par;
        lat <- (-89 to 90).par;
        loc = GridLocation(lat, lon)
      ) yield {
        (loc -> function(loc))
      }
    ).seq.toMap
  }

}

