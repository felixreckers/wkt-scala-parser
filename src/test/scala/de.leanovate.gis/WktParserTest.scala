package de.leanovate.gis

import org.scalatest._

class WktParserTest extends FunSuite {

  import WktParser._

  test("point") {
    assert(WktParser.parse("POINT(13.41145 58.67456)") === Point(13.41145, 58.67456))
  }

  test("empty point") {
    assert(WktParser.parse("POINT EMPTY") === EmptyPoint)
  }

  test("point lowercase") {
    assert(WktParser.parse("point(13.41145 58.67456)") === Point(13.41145, 58.67456))
  }


  test("point with whitespace") {
    assert(WktParser.parse(" POINT ( 13.41145  58.67456 ) ") === Point(13.41145, 58.67456))
  }

  test("linestring") {
    assert(WktParser.parse("LINESTRING(30 10,10 30,40 40)") === LineString(Point(30, 10), Point(10, 30), Point(40, 40)))
  }

  test("empty linestring") {
    assert(WktParser.parse("LINESTRING EMPTY") === LineString())
  }

  test("polygon without inner") {
    assert(WktParser.parse("POLYGON ((30 10, 40 40, 20 40, 10 20, 30 10))") === Polygon(
      LineString(Point(30, 10), Point(40, 40), Point(20, 40), Point(10, 20), Point(30, 10))))
  }

  test("polygon with inner") {
    val wkt = "POLYGON ((35 10, 45 45, 15 40, 10 20, 35 10),(20 30, 35 35, 30 20, 20 30))"
    val polygon = Polygon(
      LineString(Point(35, 10), Point(45, 45), Point(15, 40), Point(10, 20), Point(35, 10)),
      LineString(Point(20, 30), Point(35, 35), Point(30, 20), Point(20, 30))
    )
    assert(WktParser.parse(wkt) === polygon)
  }

  test("polygon with multiple inner") {
    val wkt = "POLYGON ((35 10, 45 45, 15 40, 10 20, 35 10),(20 30, 35 35, 30 20, 20 30),(35 10, 45 45, 15 40, 10 20))"
    val polygon = Polygon(
      LineString(Point(35, 10), Point(45, 45), Point(15, 40), Point(10, 20), Point(35, 10)),
      LineString(Point(20, 30), Point(35, 35), Point(30, 20), Point(20, 30)),
      LineString(Point(35, 10), Point(45, 45), Point(15, 40), Point(10, 20))
    )
    assert(WktParser.parse(wkt) === polygon)
  }

  test("empty polygon") {
    val wkt = "POLYGON EMPTY"
    assert(WktParser.parse(wkt) === EmptyPolygon)
  }

  test("multipoint") {
    val wkt = "MULTIPOINT (10 40,40 30,20 20, 30 10)"
    val multipoint = MultiPoint(Point(10, 40), Point(40, 30), Point(20, 20), Point(30, 10))
    assert(WktParser.parse(wkt) === multipoint)
  }

  test("empty multipoint") {
    val wkt = "MULTIPOINT EMPTY"
    assert(WktParser.parse(wkt) === MultiPoint())
  }

  test("multilinestring") {
    val wkt = "MULTILINESTRING ((10 10, 20 20, 10 40),(40 40, 30 30, 40 20, 30 10))"
    val multiLineString = MultiLineString(
      LineString(Point(10, 10), Point(20, 20), Point(10, 40)),
      LineString(Point(40, 40), Point(30, 30), Point(40, 20), Point(30, 10)))
    assert(WktParser.parse(wkt) === multiLineString)
  }

  test("multipolygon without inner") {
    val wkt = "MULTIPOLYGON (((30 20, 45 40, 10 40, 30 20)),((15 5, 40 10, 10 20, 5 10, 15 5)))"
    val multiPolygon = MultiPolygon(
      Polygon(LineString(Point(30,20), Point(45,40), Point(10,40), Point(30,20))),
      Polygon(LineString(Point(15,5), Point(40,10), Point(10,20), Point(5,10), Point(15,5)))
    )
    assert(WktParser.parse(wkt) === multiPolygon)
  }

  test("multipolygon with inner") {
    val wkt = "MULTIPOLYGON (((40 40, 20 45, 45 30, 40 40)),((20 35, 10 30, 10 10, 30 5, 45 20, 20 35),(30 20, 20 15, 20 25, 30 20)))"
    val multiPolygon = MultiPolygon(
      Polygon(LineString(Point(40,40), Point(20,45), Point(45,30), Point(40,40))),
      Polygon(LineString(Point(20,35), Point(10,30), Point(10,10), Point(30,5), Point(45,20), Point(20,35)),
        LineString(Point(30,20), Point(20,15), Point(20,25), Point(30, 20))))
    assert(WktParser.parse(wkt) === multiPolygon)

  }


}
