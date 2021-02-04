package observatory

import observatory.Interaction2._
import org.junit.Assert._
import org.junit.Test

trait Interaction2Test extends MilestoneSuite {
  private val milestoneTest = namedMilestoneTest("interactive user interface", 6) _

  private val yearRange = 2000 to 2030

  // yearSelection must never be out of the selected layer bounds (3pts)(observatory.CapstoneSuite)

  @Test def `yearSelection must never be less than lower bound`: Unit = {

    val sLayer = Signal( Layer(LayerName.Temperatures,null,yearRange) )
    val sliderValue = Signal(1999)
    assert(yearSelection(sLayer,sliderValue)().equals(2000),"yearSelection passes lower bound!")
  }

  @Test def `yearSelection must never be higher than upper bound`: Unit = {

    val sLayer = Signal( Layer(LayerName.Temperatures,null,yearRange) )
    val sliderValue = Signal(2031)
    assert(yearSelection(sLayer,sliderValue)().equals(2030),"yearSelection passes upper bound!")
  }

  @Test def `layerUrlPattern must never contain a year outside the layer bounds`: Unit = {

    val sLayer = Signal( Layer(LayerName.Deviations,null,yearRange) )
    val sliderValue = Signal(2040)
    val actualUrl = layerUrlPattern(sLayer,sliderValue)()
    val expectedUrl = "target/deviations/2030/{z}/{x}-{y}.png"
    assertEquals("Caption must cutoff year extending beyond year range",expectedUrl,actualUrl)
  }

  @Test def `caption must never contain a year outside the layer bounds`: Unit = {

    val sLayer = Signal( Layer(LayerName.Deviations,null,yearRange) )
    val sliderValue = Signal(2040)
    val actualUrl = caption(sLayer,sliderValue)()
    val expectedUrl = "Deviations (2030)"
    assertEquals("Caption must cutoff year extending beyond year range",expectedUrl,actualUrl)
  }
}
