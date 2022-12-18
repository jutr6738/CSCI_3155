package edu.colorado.csci3155.project2

import org.scalatest.funsuite._


class OperatorTests extends AnyFunSuite {


    def distance(c1:(Double,Double), c2: (Double,Double)) =
        math.sqrt( math.pow((c1._1 - c2._1), 2) + math.pow(c1._2-c2._2, 2))


    test("Simple Circle"){
        val s =
            """
              |let y = 10 + 15 in
              |let x = circle(y) in
              |   x
              |""".stripMargin
        val v1 = TestPrograms.parseAndInterpretProgram(s)
        v1 match {
            case FigValue(canvas) => {
                val cir1  = new MyCircle((0.0, 0.0), 25.0)
                assert (canvas.numCircles == 1)
                assert (canvas.numPolygons == 0)
                assert (canvas.getListOfObjects.contains(cir1))
            }
            case _ => assert(false, "Program evaluates to the wrong type. Expected value type is FigValue. Instead we got ${v1}")
        }
    }

    test("Simple Triangle"){
        val s =
            """
              |let y = 10  * 10 in
              |let x = triangle(y) in
              |   x
              |""".stripMargin
        val v1 = TestPrograms.parseAndInterpretProgram(s)
        v1 match {
            case FigValue(canvas) => {
                val tri  = new Polygon(List((0,0), (100,0), (50, 86.60255)))
                assert (canvas.numPolygons == 1)
                assert (canvas.numCircles == 0)
                val myTriangle = canvas.getListOfObjects.head.asInstanceOf[Polygon]
                val coordinates = myTriangle.cList
                assert(coordinates.length == 3, "Triangle must have three corner points")
                assert (coordinates.contains((0,0)), "Triangles must have (0,0) ")
                assert(coordinates.contains(100,0), "Triangle must have (100,0)")
                assert(coordinates.count( distance(_, (50, 86.60254)) <= 1E-04) == 1, "Triangle must have a point very close to (50.0,86.60254037844386)")
            }
            case _ => assert(false, "Program evaluates to the wrong type. Expected value type is FigValue. Instead we got ${v1}")
        }
    }

    test("Simple Rectangle"){
        val s =
            """
              |let y = 25 - 15 in
              |let x = rectangle(y) in
              |   x
              |""".stripMargin
        val v1 = TestPrograms.parseAndInterpretProgram(s)
        v1 match {
            case FigValue(canvas) => {
                val expectedVerts = List((0,0), (10, 0), (10, 10), (0, 10))
                assert (canvas.numCircles == 0)
                assert (canvas.numPolygons == 1)
                val myrectangle = canvas.getListOfObjects.head.asInstanceOf[Polygon]
                val coordinates = myrectangle.cList
                assert(coordinates.length == 4, "Rectangle must have four corner points")
                expectedVerts.foreach{
                    case (x, y) => assert (coordinates.contains((x,y)), s"Coordinate ${(x,y)} is missing in rectangle")
                }
            }
            case _ => assert(false, s"Program evaluates to the wrong type. Expected value type is FigValue. Instead we got ${v1}")
        }
    }

    test("Simple Line"){
        val s =
            """
              |let y = 25 - 15 in
              |let x = line(y) in
              |   x
              |""".stripMargin
        val v1 = TestPrograms.parseAndInterpretProgram(s)
        v1 match {
            case FigValue(canvas) => {
                val expectedVerts = List((0,0), (10.0, 0))
                assert (canvas.numCircles == 0)
                assert (canvas.numPolygons == 1)
                val myrectangle = canvas.getListOfObjects.head.asInstanceOf[Polygon]
                val coordinates = myrectangle.cList
                assert(coordinates.length == 2)
                assert(expectedVerts.forall( coordinates.contains(_)))
            }
            case _ => assert(false, s"Program evaluates to the wrong type. Expected value type is FigValue. Instead we got ${v1}")
        }
    }
    test("Plus Operator Test") {
        val s =
            """
              |let x = 10 + 15 in
              | let y = rectangle(x) + circle(x) in
              |   y
              |""".stripMargin
        val v1 = TestPrograms.parseAndInterpretProgram(s)
        v1 match {
            case FigValue(canvas) => {
                assert (canvas.numCircles == 1)
                assert (canvas.numPolygons == 1)
                val listOfObjects = canvas.getListOfObjects
                assert(listOfObjects.forall({
                    case MyCircle(c, r) => {
                        assert (math.abs(c._1)<= 1E-6 && math.abs(c._2) <= 1E-6, s"Center of circle wrong: expected (0,0), got $c")
                        assert ( math.abs(r - 25.0) <= 1E-6, "Radius of circle wrong: expected 25, got $r")
                        true
                    }
                    case Polygon(lst)=> {
                        val expectedCoords = List((0,0), (25, 0), (25, 25), (0, 25))
                        assert (lst.length == 4, "Rectangle must have 4 coordinates")
                        lst.forall( c => expectedCoords.count(p => math.abs(p._1 - c._1) <= 1E-06
                                                                   && math.abs(p._2 - c._2) <= 1E-06) == 1
                                  )
                    }
                }))
            }
            case _ => assert(false, s"Program evaluates to the wrong type. Expected value type is FigValue. Instead we got ${v1}")
        }
    }


    test("Star Operator Test") {
        val s =
            """
              |let x = 10 + 15 in
              | let y = rectangle(x) * circle(x) in
              |   y
              |""".stripMargin
        val v1 = TestPrograms.parseAndInterpretProgram(s)
        v1 match {
            case FigValue(canvas) => {
                assert (canvas.numCircles == 1)
                assert (canvas.numPolygons == 1)
                val listOfObjects = canvas.getListOfObjects
                assert(listOfObjects.forall({
                    case MyCircle(c, r) => {
                        assert (math.abs(c._1 - 50.0)<= 1E-6 && math.abs(c._2-12.5) <= 1E-6, s"Center of circle wrong: expected (0,0), got $c")
                        assert ( math.abs(r - 25.0) <= 1E-6, "Radius of circle wrong: expected 25, got $r")
                        true
                    }
                    case Polygon(lst)=> {
                        val expectedCoords = List((0,0), (25, 0), (25, 25), (0, 25))
                        assert (lst.length == 4, "Rectangle must have 4 coordinates")
                        lst.forall( c => expectedCoords.count(p => math.abs(p._1 - c._1) <= 1E-06
                          && math.abs(p._2 - c._2) <= 1E-06) == 1
                        )
                    }
                }))
            }
            case _ => assert(false, s"Program evaluates to the wrong type. Expected value type is FigValue. Instead we got ${v1}")
        }
    }

    test("Slash Operator Test") {
        val s =
            """
              |let x = 10 + 15 in
              | let y = rectangle(x) / circle(x) in
              |   y
              |""".stripMargin
        val v1 = TestPrograms.parseAndInterpretProgram(s)
        v1 match {
            case FigValue(canvas) => {
                assert (canvas.numCircles == 1)
                assert (canvas.numPolygons == 1)
                val listOfObjects = canvas.getListOfObjects
                assert(listOfObjects.forall({
                    case MyCircle(c, r) => {
                        assert (math.abs(c._1 )<= 1E-6 && math.abs(c._2) <= 1E-6, s"Center of circle wrong: expected (0,0), got $c")
                        assert ( math.abs(r - 25.0) <= 1E-6, "Radius of circle wrong: expected 25, got $r")
                        true
                    }
                    case Polygon(lst)=> {
                        val expectedCoords = List((-12.5,25.0), (12.5,25.0), (12.5, 50.0), (-12.5, 50.0))
                        assert (lst.length == 4, "Rectangle must have 4 coordinates")
                        lst.forall( c => expectedCoords.count(p => math.abs(p._1 - c._1) <= 1E-06
                          && math.abs(p._2 - c._2) <= 1E-06) == 1
                        )
                    }
                }))
            }
            case _ => assert(false, s"Program evaluates to the wrong type. Expected value type is FigValue. Instead we got ${v1}")
        }
    }
}
