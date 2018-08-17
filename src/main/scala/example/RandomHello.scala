package example

import java.io.File
import java.util
import java.util.Random

import Cartesian._

import scala.annotation.tailrec
import scala.language._
import scala.collection.generic.CanBuildFrom
import scala.io.Source

object RandomHello extends App with RandomSkiMapper with RandomSkiRouter {

  val gBound = GraphBound(1, 1000)
  val vBound = ValueBound(1, 1500)


  val results = /*start*/ run/*ning!*/

  println(greeting)
  println(results.map(_.value).mkString("-"))
}

trait RandomSkiMapper {
  lazy val greeting: String = "hello"

  val intRandom = new Random()

  def gBound: GraphBound
  def vBound: ValueBound
  def pad: Vertex = Vertex(0, 0, 0)

  def run: Seq[Vertex]

  def graphOffset: Int = {
    //plain simple offset from zero
    val upper = math.abs(gBound.upper) - math.abs(gBound.lower)
    //possible error conversion from user double/long to int
    upper.toInt
  }

  /**
    * Generate pad for sliding first and last
    * @return
    */
  def generatePad(fill: Int = 1): Vector[Vertex] = Vector.fill(fill)(pad)

  /**
    * Generating data set by row of size == gBound.upper
    *
    * @param yIndex Y-Index
    * @return An array of vertex
    */
  def generateDataset(yIndex: Int): Vector[Vertex] = {
   (0 to graphOffset).map(x => nextVertex(x, yIndex)).toVector
  }

  /**
    * New Vertex from random Int generation with lower bound inclusive and upper bound exclusive
    * 1 to 1500 => 1, 2, ... 1499
    *
    * @param x
    * @param y
    * @return
    */
  def nextVertex(x: Int, y: Int, value: Option[Int] = None): Vertex = {
    val vertexValue = if (value.nonEmpty) value.get else vBound.lower + intRandom.nextInt(vBound.upper - vBound.lower)
    Vertex(x, y, vertexValue)
  }
}


final case class GraphBound(lower: Int, upper: Int)
final case class ValueBound(lower: Int, upper: Int)

trait RandomSkiRouter {
  this: RandomSkiMapper =>

  def run: Seq[Vertex] = {
    val data = dataset(gBound.upper)
    val routes = slideThrough(data)
    val maxTrailSize = routes.maxBy(_.trail.size).trail.size
    routes.filter(_.trail.size == maxTrailSize).maxBy(_.steepFactor).vertexTrail
  }




  def dataset(limit: Int): Stream[Vector[Vertex]] = {
    import scala.collection.JavaConverters._

    val numRows = limit + 1
    var cursor = 0
    val buffer = new util.ArrayList[Vector[Vertex]]()

    while (cursor <= numRows){
      if(cursor == 0 || cursor == numRows){
        buffer.add(generatePad(gBound.upper))
      } else buffer.add(generateDataset(cursor))
      cursor = cursor + 1
    }
    buffer.asScala.toStream
  }

  /**
    * Run thRandom Vertexes
    *
    * @return
    */
  def slideThrough(dataset: Stream[Vector[Vertex]]): Vector[SkiRoute] = {
    dataset.slideLeft(3)(Vector[SkiRoute]()){(res, data) =>
      val formed = formulateRoutes(data)
      val hrz = linkHorizontalRoutesV2(formed)
      linkVerticalRoutesV2(res, hrz)
    }
  }

  /**
    * Concat Vertexes to basic Edges to simple SkiRoutes
    *
    * @param data vertexes
    * @return
    */
  def formulateRoutes(data: Stream[Vector[Vertex]]): Vector[SkiRoute] = {
    val top = data.toVector(0)
    val ctx = data.toVector(1)
    val low = data.toVector(2)
    //create edges
    ctx.flatMap{c =>
      //for each vertex there are 4 possible ski edges

      val northEdge = if(top(c.x) == pad || top(c.x) > c) None else Some(Edge(c, top(c.x), North))
      val southEdge = if(low(c.x) == pad || low(c.x) > c) None else Some(Edge(c, low(c.x), South))
      val westEdge = if(c == ctx.head || ctx(c.west) > c) None else Some(Edge(c, ctx(c.west), West))
      val eastEdge = if(c == ctx.last || ctx(c.east) > c) None else Some(Edge(c, ctx(c.east), East))

      northEdge.map(n => SkiRoute(Seq(n))).toVector ++
        southEdge.map(n => SkiRoute(Seq(n))).toVector ++
        westEdge.map(n => SkiRoute(Seq(n))).toVector ++
        eastEdge.map(n => SkiRoute(Seq(n))).toVector
    }
  }


  /**
    * Link easts and west routes if can connect to
    *
    * @param data Routes data
    */
  @tailrec
  final def linkHorizontalRoutes(data: Vector[Either[SkiRoute, SkiRoute]]): Vector[Either[SkiRoute, SkiRoute]] = {
    val linked = data.flatMap {
        case Right(v) =>
          val joints = data.filter(d => d.isRight && d.exists(v.canConnectTo))
          if (joints.nonEmpty){
            joints.flatMap(j => Vector(j.swap, j.right.map(v ~ _)))
          } else Vector(Right(v))
        case leftv => Vector(leftv)
    }

    //val dataLeft = linked.filter(_.isLeft).map(_.left.get)
    //val refined = linked.filterNot(l => l.isLeft || l.right.exists(r => dataLeft.exists(x => x.start == r.start && x.end == r.end)))

    if(linked.size == data.size) linked else linkHorizontalRoutes(linked)
  }

  @tailrec
  final def linkHorizontalRoutesV2(data: Vector[SkiRoute]): Vector[SkiRoute] = {
    val linked = data.flatMap{v =>
      val joints = data.filter(v.canConnectTo)
      if (joints.nonEmpty) joints.map(v ~ _) else Vector(v)
    }

    if(linked.size == data.size) linked else linkHorizontalRoutesV2(linked)
  }



  /**
    * Link north and south routes if can connect to
    *
    * @return
    */
  def linkVerticalRoutes(routes: Vector[SkiRoute], data: Vector[Either[SkiRoute, SkiRoute]]): Vector[SkiRoute] = {
    //Assume everything isRight
    if (routes.nonEmpty) {
      val vLinked = routes.flatMap { v =>
        val vJoints = data.filter(d => d.isRight && d.exists(v.canConnectTo))
        if(vJoints.nonEmpty) {
          vJoints.flatMap(j => Vector(j.swap, j.right.map(v ~ _)))
        } else Vector(Right(v))
      }
      val linked = vLinked ++ data
      //val fLinked = formatShortEnds(linked) TODO: A function to clean up apparent short routes that would be useless to optimize mem
      val dataLeft = linked.filter(_.isLeft).map(_.left.get)
      val refined = linked.filterNot(l => l.isLeft || l.right.exists(r => dataLeft.exists(x => x.start == r.start && x.end == r.end)))

      refined.map(_.right.get)
    } else data.map(_.right.get)
  }

   def linkVerticalRoutesV2(routes: Vector[SkiRoute], data: Vector[SkiRoute]): Vector[SkiRoute] = {
    //Assume everything isRight
    if (routes.nonEmpty) {
      val vLinked = routes.flatMap { v =>
        val joints = data.filter(v.canConnectTo).map(v ~ _)
        if(joints.nonEmpty) joints else Vector(v)
      }
      vLinked ++ data
      //val fLinked = formatShortEnds(linked) TODO: A function to clean up apparent short routes that would be useless to optimize mem
    } else data
  }


  /**
    * Implementation of foldLeft and sliding which is used to slide data rows
    * and linking previous SkiRoutes to context SkiRoutes
    *
    * @tparam S A collection, sub type of Seq
    * @tparam T Any type T, SkiRoute
    */
  trait SlideLeftLike [S[A] <: Seq[A], T] {

    def s: S[T]

    def slideLeft[R](take: Int = 1)(r: R)(block: (R, S[T]) => R)(implicit cb: CanBuildFrom[Nothing, T, S[T]]): R = {
      val size = s.size
      var cursor: Int = take
      var result: R = r

      while (cursor <= size) {
        val nextLoad = s.slice(cursor - take, cursor)
        result = block(result, nextLoad.to[S])
        cursor = cursor + 1
      }
      result
    }
  }

  implicit class StreamSlideLeft[T](val s: Stream[T]) extends SlideLeftLike[Stream, T]
  implicit class VectorSlideLeft[T](val s: Vector[T]) extends SlideLeftLike[Vector, T]



  /*def asEither[F[A] <: Seq[A], T](v: F[T])(implicit cb: CanBuildFrom[F[T], Either[T, T], F[Either[T, T]]]): F[Either[T, T]] = {
    v.map[Either[T, T], F[Either[T, T]]](Right(_))
  }*/

  /**
    * Converts a Vector[SkiRoutes] to a  Vector[Either[SkiRoutes, SkiRoutes]
    *
    * @return
    */
  implicit def asVectorEither[R](v: Vector[R]): Vector[Either[R, R]] = v.map(Right(_))

}