package example

import org.scalatest.{FlatSpec, Inspectors, Matchers}

class RandomSkiSpec extends FlatSpec with Matchers with Inspectors {

  val gBound: GraphBound = GraphBound(1, 4)
  val vBound: ValueBound = ValueBound(0, 0)

  val testData = Vector(
    Vector(4, 8, 7, 3),
    Vector(2, 5, 9, 3),
    Vector(6, 3, 2, 5),
    Vector(4, 4, 1, 6)
  )

  val TestResult = "9-5-3-2-1"

  val dataSet = testData.zipWithIndex
    .map{case (s, y) => s.zipWithIndex.map{case (v, x) =>
      val idx = y + 1
      RandomHello.nextVertex(x, idx, Some(v))}
    }

  val testDataset = Stream(RandomHello.generatePad(gBound.upper) +: dataSet :+ RandomHello.generatePad(gBound.upper): _*)


  /*"At most 4 SkiRoutes" should "be formed for each vertex point " in {
    forAll(testData){v =>
      forAll(v){t =>
        val vertex = Stream(Vector(Hello.nextVertex(1, 1, Some(t))))
        val result = Hello.formulateRoutes(vertex)
        result.size should be <= 4
      }
    }
  }*/


  TestResult should "be the long-steepest route" in new RandomSkiMapper with RandomSkiRouter {

    override def gBound: GraphBound = GraphBound(1, 4)

    override def vBound: ValueBound = ValueBound(0, 0)


    override def run: Seq[Vertex] = {
      val routes = slideThrough(testDataset)
      val maxTrailSize = routes.maxBy(_.trail.size).trail.size
      routes.filter(_.trail.size == maxTrailSize).maxBy(_.steepFactor).vertexTrail
    }

    val result = run.map(_.value).mkString("-")
    assert(result == TestResult)
  }

}

