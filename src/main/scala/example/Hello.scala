package example

import java.io.File
import java.net.URL

import Cartesian._

import scala.annotation.tailrec
import scala.language._
import scala.collection.generic.CanBuildFrom
import scala.io.Source

object Hello extends App with SkiMapper with SkiRouter {

  val isLocalFile: Boolean = false

  val mapDataUri = "https://s3-ap-southeast-1.amazonaws.com/geeks.redmart.com/coding-problems/map.txt"

  val results = /*start*/ run/*ning!*/

  println("Longest steepest route: \n\t")
  println(results.map(_.value).mkString("-"))
}

trait SkiMapper {

  def isLocalFile: Boolean

  def mapDataUri: String

  def run: Seq[Vertex]


}

trait SkiRouter {
  this: SkiMapper =>

  def run: Seq[Vertex] = {
    val routes = slideThroughV2
    val maxTrailSize = routes.maxBy(_.trail.size).trail.size
    val longRoutes = routes.filter(_.trail.size == maxTrailSize).sortBy(_.steepFactor)
    longRoutes.map(_.vertexTrail.map(_.value).mkString("-")).foreach(println)
    longRoutes.last.vertexTrail
  }

  def dataLines: Stream[(String, Int)] = {
    println("Fetching data file")

    val source = if (isLocalFile) Source.fromFile(new File(mapDataUri)) else Source.fromURL(mapDataUri)

    source.getLines().toStream.tail.zipWithIndex
  }

  /**
    * Pad data lines to slide in context over first and last row
    * @param dataStream
    * @return
    */
  def padData(dataStream: Stream[(String, Int)]): Stream[(String, Int)] = {
    val padString = Stream.fill(1000)(0).mkString(" ")
    Stream((padString, 0)) ++
      dataStream.map{case (str, int) => (str, int + 1)} ++
      Stream((padString, dataStream.size + 1))
  }

  /**
    * No recovery if any of data points isnt an int
    * @param data
    * @return
    */
  def toVertex(data: (String, Int)) = {
    data match {
      case (str, y) =>
        str.split(" ").toVector.zipWithIndex.map{case (v, x) => Vertex(x, y, v.toInt) }
    }
  }

  def printProgress(lines: Int, data: Option[Vertex]): Unit = {
    val percent: Double = data.map(v => (v.y / lines.toDouble) * 100 ).getOrElse(0)
    val line = data.map(v => v.y ).getOrElse(0)
    val message = f"Mine level: $percent%1.2f%%, $line of $lines lines"
   println(message)
  }


  /**
    * Run through Vertexes
    *
    * @return
    */
  def slideThrough: Vector[SkiRoute] = {
    val lines = dataLines
    val linesSize = lines.size

    padData(lines).map(toVertex).slideLeft(3)(Vector[SkiRoute]()){(res, data) =>
      val formed = formulateRoutes(data = data, ignoreYIndex = List(0, linesSize + 1))
      val hrz = linkHorizontalRoutesV2(formed)
      val verticals = linkVerticalRoutesV2(res, hrz)
      printProgress(linesSize, data.headOption.flatMap(_.headOption))
      verticals
    }
  }

  /**
    * Run through Vertexes
    *
    * @return
    */
  def slideThroughV2: Vector[SkiRoute] = {
    val lines = dataLines
    val linesSize = lines.size
    println(s"Generating horizontal routes for $linesSize data lines")
    val horizontalRoutes = padData(lines).map(toVertex).sliding(3, 1).map { v =>
      val formed = formulateRoutes(data = v, ignoreYIndex = List(0, linesSize + 1))
      printProgress(linesSize, formed.headOption.map(_.start))
      linkHorizontalRoutesV2(formed)
    }
    val hRoutes = horizontalRoutes.toVector

    println("Linking vertical routes")
    routeVertical(hRoutes).flatten
  }

  /**
    * Link vertical routes
    *
    * @return
    */
  @tailrec
  private def routeVertical(routes: Vector[Vector[SkiRoute]]): Vector[Vector[SkiRoute]] = {
    println(s"Reducing vertical routes from ${routes.size}")
    val linked = routes.sliding(2, 2).toParArray.map(_.foldLeft(Vector[SkiRoute]())(linkVerticalRoutesV2)).toVector
    if(linked.size != 1) routeVertical(linked) else linked
  }

  /**
    * Concat Vertexes to basic Edges to simple SkiRoutes
    * TODO: Error boundaries
    *
    * @param data vertexes
    * @return
    */
  def formulateRoutes(data: Stream[Vector[Vertex]], ignoreYIndex: Seq[Int] = Seq()): Vector[SkiRoute] = {
    val top = data.head
    val ctx = data(1)
    val low = data.last

    /**
      * Avoid null/invalid index references
      * If function is less than 14bytes, would be JIT inlined
      * @param op index operation
      * @return
      */
    def getOrTrue(op: => Boolean): Boolean = try { op } catch {case _: Exception => true }

    //create edges
    ctx.flatMap{c =>
      //for each vertex there are 4 possible ski edges

      val northEdge = if(ignoreYIndex.contains(top(c.x).y) || getOrTrue(top(c.x) > c)) None else Some(Edge(c, top(c.x), North))
      val southEdge = if(ignoreYIndex.contains(low(c.x).y) || getOrTrue(low(c.x) > c)) None else Some(Edge(c, low(c.x), South))
      val westEdge = if(c == ctx.head || ctx(c.west) > c) None else Some(Edge(c, ctx(c.west), West))
      val eastEdge = if(c == ctx.last || ctx(c.east) > c) None else Some(Edge(c, ctx(c.east), East))

      northEdge.map(n => SkiRoute(Vector(n))).toVector ++
        southEdge.map(n => SkiRoute(Vector(n))).toVector ++
        westEdge.map(n => SkiRoute(Vector(n))).toVector ++
        eastEdge.map(n => SkiRoute(Vector(n))).toVector
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

  /**
    * Link easts and west routes if can connect to
    * Version 2: Allows multiplexed routes
    *
    * @param data
    * @return
    */
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

  /**
    * Link north and south routes if can connect to
    * Version 2: Allows multiplex routes
    *
    * @param routes
    * @param data
    * @return
    */
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