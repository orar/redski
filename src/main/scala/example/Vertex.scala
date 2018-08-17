package example


final case class Vertex(x: Int, y: Int, value: Int) extends Ordered[Vertex] {

  override def compare(that: Vertex): Int = {
    java.lang.Integer.valueOf(value).compareTo(java.lang.Integer.valueOf(that.value))
  }

  /**
    * Find the next value y-index north/south
    * @return
    */
  def north: Int = y - 1
  def south: Int = y + 1

  /**
    * Find the next value x-index west/east
    * @return
    */
  def west: Int = x - 1
  def east: Int = x + 1

  /**
    * The index of this vertex
    * if the (x, y) vertex of this vertex is  (4, 9), the index is 49
    * which differentiates from (9, 4) which is 94
    *
    * @return
    */
  def index: Int = {
    val idxString = x.toString + y.toString
    idxString.toInt
  }

  /**
    * Find the north vertex index
    * @return
    */
  def northIndex: Int = {
    val idxString = x.toString + north.toString
    idxString.toInt
  }

  /**
    * Find the north vertex index
    * @return
    */
  def southIndex: Int = {
    val idxString = x.toString + south.toString
    idxString.toInt
  }

  /**
    * Find the west vertex index
    * @return
    */
  def westIndex: Int = {
    val idxString = west.toString + y.toString
    idxString.toInt
  }

  /**
    * Find the east vertex index
    * @return
    */
  def eastIndex: Int = {
    val idxString = east.toString + y.toString
    idxString.toInt
  }


}

object Cartesian extends Enumeration {
  type Point = Value

  val North, South, East, West = Value
}

final case class Edge(from: Vertex, to: Vertex, direction: Cartesian.Point) {

  def canConnectTo(edge: Edge): Boolean = {

    to.x == edge.from.x && to.y == edge.from.y && to.value == edge.from.value && canConnectEdge(edge)

  }

  /**
    * Recurring vertex on same axis may force routing into infinite loop. Ie East to west to east to west ...
    * Edge cant connect back in opposite direction
    * @param edge
    * @return
    */
  def canConnectEdge(edge: Edge): Boolean = {
    import Cartesian._

    !((direction == East && edge.direction == West) ||
      (direction == West && edge.direction == East) ||
       (direction == North && edge.direction == South) ||
      (direction == South && edge.direction == North)
      )
  }

}


/**
  * A set of edges representing the route
  * A single Vertex can generate 4 possible SkiRoutes
  *
  * @param trail Route of interconnected edges. Careful.. Trail is append only. No prepend
  */
final case class SkiRoute(trail: Seq[Edge]) {

  def start: Vertex = trail.head.from

  def end: Vertex = trail.last.to

  def first: Edge = trail.head

  def last: Edge = trail.last

  def ~ (that: SkiRoute): SkiRoute = {
    if (end == that.start) SkiRoute(trail ++ that.trail) else this
  }

  def canConnectTo(that: SkiRoute): Boolean = {
    end.x == that.start.x &&
    end.y == that.start.y &&
    end.value == that.start.value &&
    last.canConnectEdge(that.first)
  }

  def canConnectBack(that: SkiRoute): Boolean = {
    start.x == that.end.x &&
    start.y == that.end.y &&
    start.value == that.end.value &&
    last.canConnectEdge(that.first)
  }

  def vertexTrail: Seq[Vertex] = {
    if (trail.nonEmpty) {
      val head = trail.head.from
      val tail = trail.map(_.to)
      head +: tail
    } else Seq[Vertex]()
  }

  def steepFactor: Int = {
    val head = trail.head.from
    val tail = trail.last.to
    head.value - tail.value
  }
}