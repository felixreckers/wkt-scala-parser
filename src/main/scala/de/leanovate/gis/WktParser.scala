package de.leanovate.gis

import scala.util.parsing.combinator.JavaTokenParsers



/**
 * A simple parser for two-dimension geometry WKT using Scala parser combinators.
 * http://www.opengeospatial.org/standards/sfa
 */
object WktParser extends JavaTokenParsers {

  trait Geometry {
    def isEmpty: Boolean
  }

  sealed case class Point(x: Double, y: Double) extends Geometry {
    def isEmpty = false
  }

  object EmptyPoint extends Geometry {
    def isEmpty = true
  }

  sealed case class LineString(points: Point*) extends Geometry {
    def isEmpty = points.length == 0
  }

  sealed case class Polygon(outer: LineString, inner: LineString*) extends Geometry {
    def isEmpty = false
  }

  object EmptyPolygon extends Geometry {
    def isEmpty = true
  }

  sealed case class MultiPoint(points: Point*) extends Geometry {
    def isEmpty = points.length == 0
  }

  sealed case class MultiLineString(lineStrings: LineString*) extends Geometry {
    def isEmpty = lineStrings.length == 0
  }

  sealed case class MultiPolygon(polygons: Polygon*) extends Geometry {
    def isEmpty = polygons.length == 0
  }


  def parse(wkt: String): Geometry = {
    val result = parseAll(geometry, wkt)
    println(result)
    result.get
  }

  private def geometry: Parser[Geometry] = point | emptyPoint | lineString  | polygon | emptyPolygon | multiPoint | multiLineString | multiPolygon

  private def point: Parser[Point] = caseInsensitive("POINT") ~> "(" ~> coordinate <~ ")"

  private def lineString: Parser[LineString] = caseInsensitive("LINESTRING") ~> (coordinateSeq | emptyLineString)

  private def polygon: Parser[Polygon] = caseInsensitive("POLYGON") ~> polygonContent

  private def polygonContent: Parser[Polygon] = "(" ~> coordinateSeq ~ inner <~ ")" ^^ {
    case outer ~ inner => Polygon(outer, inner.getOrElse(Nil):_*)
  }

  private def multiPoint: Parser[MultiPoint] = caseInsensitive("MULTIPOINT") ~> (coordinateSeq | emptyLineString)  ^^ {
    case lineString => if(lineString.isEmpty) MultiPoint() else MultiPoint(lineString.points:_*)
  }

  private def multiLineString: Parser[MultiLineString] = caseInsensitive("MULTILINESTRING") ~> "(" ~> coordinatesSeq <~ ")" ^^ (MultiLineString(_:_*))

  private def multiPolygon: Parser[MultiPolygon] = caseInsensitive("MULTIPOLYGON") ~> "(" ~> repsep(polygonContent, ",") <~ ")" ^^ {
    case polygons => (MultiPolygon(polygons:_*))
  }

  private def coordinatesSeq: Parser[List[LineString]] = repsep(coordinateSeq, ",")

  private def inner: Parser[Option[List[LineString]]] = opt("," ~> repsep(coordinateSeq, ","))

  private def coordinateSeq: Parser[LineString] = "(" ~> repsep(coordinate, ",") <~ ")" ^^ (LineString(_:_*))

  private def coordinate: Parser[Point] = num ~ num ^^ {
    case x ~ y => Point(x, y)
  }

  private def num: Parser[Double] = floatingPointNumber ^^ (_.toDouble)

  private def caseInsensitive(str: String): Parser[String] = ("""(?i)\Q""" + str + """\E""").r

  private def empty: Parser[String] = caseInsensitive("EMPTY")

  private def emptyLineString: Parser[LineString] = empty ^^ (_ => LineString())

  private def emptyPoint: Parser[EmptyPoint.type] = caseInsensitive("POINT") ~> empty ^^ (_ => EmptyPoint)

  private def emptyPolygon: Parser[EmptyPolygon.type] = caseInsensitive("POLYGON") ~> empty ^^ (_ => EmptyPolygon)

}
