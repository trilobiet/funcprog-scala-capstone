package observatory

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

    val rTemperatures = temperatures.map { case(loc,temp) => (loc.asRadians,temp) }

    val gridTemperatures: Map[GridLocation, Temperature] = {
      for (
        x <- (-89 to 90).par;
        y <- (-180 to 179).par;
        loc = GridLocation(x, y)
      ) yield
        (loc -> Visualization.predictTemperature(rTemperatures, loc.asRadians))
    }.seq.toMap

    val grid = TemperatureGrid(gridTemperatures)

    // return requested function
    grid.get
  }

  /**
    * @param temperaturess Sequence of known temperatures over the years (each element of the collection
    *                      is a collection of pairs of location and temperature)
    * @return A function that, given a latitude and a longitude, returns the average temperature at this location
    */
  def average(temperaturess: Iterable[Iterable[(Location, Temperature)]]): GridLocation => Temperature = {

    val years = temperaturess.size
    val q: Iterable[GridLocation => Temperature] = temperaturess.map(yearData => makeGrid(yearData))

    // sum all temperatures for this grid location and divide by years
    def f(loc:GridLocation):Temperature = {
      val tSum = q.par.foldLeft(0.0)((t: Temperature, f: GridLocation => Temperature) => t + f(loc))
      val tAvg = tSum / years
      tAvg
    }

    val avgTemperatures: Map[GridLocation, Temperature] = gridLoop(f)

    /*
    val avgTemperatures: Map[GridLocation, Temperature] = {
      for (
        x <- -89 to 90;
        y <- -180 to 179;
        loc = GridLocation(x, y)
      ) yield {
        // sum all temperatures for this grid location and divide by years
        val tSum = q.par.foldLeft(0.0)((t: Temperature, f: GridLocation => Temperature) => t + f(loc))
        val tAvg = tSum/years
        (loc -> tAvg)
      }
    }.toMap
    */

    val grid = TemperatureGrid(avgTemperatures)

    // return requested function
    grid.get
  }

  /**
    * @param temperatures Known temperatures
    * @param normals A grid containing the “normal” temperatures
    * @return A grid containing the deviations compared to the normal temperatures
    */
  def deviation(temperatures: Iterable[(Location, Temperature)], normals: GridLocation => Temperature): GridLocation => Temperature = {

    val gridTemperatures = makeGrid(temperatures)
    for (
      x <- -89 to 90;
      y <- -180 to 179;
      loc = GridLocation(x, y)
    ) yield {

    }

  }


  def gridLoop(f:GridLocation=>Temperature): Map[GridLocation, Temperature] = {

    (
      for (
        x <- (-89 to 90).par;
        y <- (-180 to 179).par;
        loc = GridLocation(x, y)
      ) yield {
        (loc -> f(loc))
      }
    ).seq.toMap
  }


}

