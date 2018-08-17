package example

import org.scalatest.{FlatSpec, Inspectors, Matchers}

class SkiSpec extends FlatSpec with Matchers with Inspectors {

  /**
    * Long-steepest test result
    */
  val TestResult = "9-5-3-2-1"



  TestResult should "be the long-steepest route" in new SkiMapper with SkiRouter {


    override def isLocalFile: Boolean = true

    override def mapDataUri: String = "/home/orar/Documents/testredmap.txt"

    override def run: Seq[Vertex] = {
      val routes = slideThrough
      val maxTrailSize = routes.maxBy(_.trail.size).trail.size
      routes.filter(_.trail.size == maxTrailSize).maxBy(_.steepFactor).vertexTrail
    }

    val result = run.map(_.value).mkString("-")
    assert(result == TestResult)
  }


  //"Unequal data lines/rows should be able to find the long steepest route"



}

