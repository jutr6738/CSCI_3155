package edu.colorado.csci3155.project2

/* A class to maintain a canvas. */
import java.awt.geom.{Ellipse2D, Rectangle2D}
import java.awt.{Graphics2D}

/* A figure is a sealed trait. It can be a Polygon or a "MyCircle"*/
sealed trait Figure {
    def getBoundingBox: (Double, Double, Double, Double)
    def translate(shiftX: Double, shiftY: Double): Figure
    def rotate(angRad: Double): Figure
    def render(g: Graphics2D, scaleX: Double, scaleY: Double, shiftX: Double, shiftY: Double): Unit
}

/*
 Class Polygon
   A polygon is defined by a list of its vertices
 */

case class Polygon(val cList: List[(Double, Double)]) extends Figure {
    //TODO: Define the bounding box of the polygon
    override def getBoundingBox: (Double, Double, Double, Double ) = {
        val xmin= cList(0)._1
        val xmax= cList(0)._1
        val ymin= cList(0)._2
        val ymax= cList(0)._2
        cList.foldLeft[(Double, Double, Double, Double)]((xmin, xmax, ymin, ymax)) ((acc, coord) => {
            val minx= math.min(acc._1, coord._1)
            val maxx= math.max(acc._2, coord._1)
            val miny= math.min(acc._3, coord._2)
            val maxy= math.max(acc._4, coord._2)
            (minx,maxx,miny,maxy)
        })
    }
    //TODO: Create a new polygon by shifting each vertex in cList by (x,y)
    //    Do not change the order in which the vertices appear
    override def translate(shiftX: Double, shiftY: Double): Polygon = {
        val shiftList: List[(Double,Double)] = cList.map( coord => (coord._1+shiftX, coord._2+shiftY)  )
        val poly= new Polygon(shiftList)
        poly
    }

    //rotates the polygon by angRad using 𝑥′=𝑥cos(𝜃)−𝑦sin(𝜃),   𝑦′=𝑥sin(𝜃)+𝑦cos(𝜃)
    override def rotate(angRad: Double): Polygon = {
        val shiftList: List[(Double,Double)] = cList.map( coord => {
            val origXc=coord._1
            val origY=coord._2
            val rotX=origXc*math.cos(angRad)-origY*math.sin(angRad)
            val rotY= origXc*math.sin(angRad)+origY*math.cos(angRad)
            (rotX,rotY)
        }  )
        val rotPoly= new Polygon(shiftList)
        rotPoly
    }

    // Function: render -- draw the polygon. Warning: DO NOT EDIT THIS FUNCTION.
    override def render(g: Graphics2D, scaleX: Double, scaleY: Double, shiftX: Double, shiftY: Double) = {
        val xPoints: Array[Int] = new Array[Int](cList.length)
        val yPoints: Array[Int] = new Array[Int](cList.length)
        for (i <- 0 until cList.length){
            xPoints(i) = ((cList(i)._1 + shiftX )* scaleX).toInt
            yPoints(i) = ((cList(i)._2 + shiftY) * scaleY).toInt
        }
        g.drawPolygon(xPoints, yPoints, cList.length)
    }
}

/*
  Class MyCircle
  Define a circle with a given center c and radius r
 */
case class MyCircle(val c: (Double, Double), val r: Double) extends Figure {
    //TODO: Define the bounding box for the circle
    override def getBoundingBox: (Double, Double, Double, Double) = {
        val xmin= c._1-r;
        val xmax= c._1+r;
        val ymin= c._2-r;
        val ymax= c._2+r;
        (xmin, xmax, ymin, ymax)
    }


    //TODO: Create a new circle by shifting the center
    override def translate(shiftX: Double, shiftY: Double): MyCircle = {
        val cx= c._1;
        val cy= c._2;
        val cir= new MyCircle((cx+shiftX,cy+shiftY),r)
        cir
    }

    //rotates the circle by angRad using 𝑥′=𝑥cos(𝜃)−𝑦sin(𝜃),   𝑦′=𝑥sin(𝜃)+𝑦cos(𝜃)
    override def rotate(angRad: Double): MyCircle = {
        val origX=c._1
        val origY=c._2
        val rotX=origX*math.cos(angRad)-origY*math.sin(angRad)
        val rotY= origX*math.sin(angRad)+origY*math.cos(angRad)
        val rotCir= new MyCircle( (rotX, rotY), r)
        rotCir
    }

    // Function: render -- draw the polygon. Do not edit this function.
    override def render(g: Graphics2D, scaleX: Double, scaleY: Double, shiftX: Double, shiftY: Double) = {
        val centerX = ((c._1 + shiftX) * scaleX) .toInt
        val centerY = ((c._2 + shiftY) * scaleY) .toInt
        val radX = (r * scaleX).toInt
        val radY = (r * math.abs(scaleY)).toInt
        //g.draw(new Ellipse2D.Double(centerX, centerY, radX, radY))
        g.drawOval(centerX-radX, centerY-radY, 2*radX, 2*radY)
    }
}

/*
  Class : MyCanvas
  Define a canvas through a list of figure objects. Figure objects can be circles or polygons.
 */
class MyCanvas (val listOfObjects: List[Figure]) {
    // TODO: Write a function to get the boundingbox for the entire canvas.
    // Hint: use existing boundingbox functions defined in each figure.
    def getBoundingBox: (Double, Double, Double, Double) = {
        val firstele= listOfObjects(0).getBoundingBox
        listOfObjects.foldLeft[(Double, Double, Double, Double)](firstele) ((acc, shape) => {
            val c= shape.getBoundingBox
            val minx= math.min(acc._1, c._1)
            val maxx= math.max(acc._2, c._2)
            val miny= math.min(acc._3, c._3)
            val maxy= math.max(acc._4, c._4)
            (minx,maxx,miny,maxy)
        })
    }


    //TODO: Write a function to translate each figure in the canvas by shiftX, shiftY
    def translate(shiftX: Double, shiftY: Double): MyCanvas = {
        val shiftFigList: List[Figure] = listOfObjects.map( shape=> shape.translate(shiftX,shiftY) )
        new MyCanvas(shiftFigList)
    }

    //TODO: Write a function that will return a new MyCanvas object that places
    // all the objects in myc2 to the right of the objects in this MyCanvas.
    // refer to the notebook documentation on how to perform this.
    def placeRight(myc2: MyCanvas):MyCanvas = {
        val (xmin1, xmax1, ymin1, ymax1)= this.getBoundingBox
        val (xmin2, xmax2, ymin2, ymax2)= myc2.getBoundingBox

        //Define  𝑥𝑆ℎ𝑖𝑓𝑡=(𝑥max,1−𝑥min,2)
        val Xshift= (xmax1-xmin2)
        //Define  𝑦𝑆ℎ𝑖𝑓𝑡=(𝑦max,1+𝑦min,1)/2−(𝑦max,2+𝑦min,2)/2 .
        val Yshift= (ymax1+ymin1)/2- (ymax2+ymin2)/2

        //translate the objects in canvas c2 by shiftX and shifY
        val transCanvas= myc2.translate(Xshift, Yshift)

        //make a new canvas that combines the objects in both
        overlap(transCanvas)
    }

    //TODO: Write a function that will return a new MyCanvas object that places
    // all the figures in myc2 on top of the figures in this MyCanvas.
    // refer to the notebook documentation on how to perform this.
    def placeTop(myc2: MyCanvas): MyCanvas = {
        val (xmin1, xmax1, ymin1, ymax1)= this.getBoundingBox
        val (xmin2, xmax2, ymin2, ymax2)= myc2.getBoundingBox
        //Define  𝑥𝑆ℎ𝑖𝑓𝑡=(𝑥max,1+𝑥min,1)/2−(𝑥max,2+𝑥min,2)/2
        val Xshift= (xmax1+xmin1)/2- (xmax2+xmin2)/2
        //Define  𝑦𝑆ℎ𝑖𝑓𝑡=(𝑦max,1−𝑦min,2)
        val Yshift= (ymax1-ymin2)

        //translate the objects in canvas c2 by shiftX and shifY
        val transCanvas= myc2.translate(Xshift, Yshift)

        //make a new canvas that combines the objects in both
        overlap(transCanvas)

    }

    //TODO: Write a function that will rotate each figure in the canvas using
    // the angle `ang` defined in radians.
    // Suggestion: first write rotation functions for polygon and circle.
    //             those functions have not been added in the classes but you can do so with the
    //             appropriate signature.
    // rotating a polygon is simply rotating each vertex.
    // rotating a circle is simply rotating the center with radius unchanged.
    def rotate(angRad: Double): MyCanvas = {
        val rotList: List[Figure]= listOfObjects.map( fig => fig.rotate(angRad) )
        new MyCanvas(rotList)
    }

    // Function to draw the canvas. Do not edit.
    def render(g: Graphics2D, xMax: Double, yMax: Double) = {
        val (lx1, ux1, ly1, uy1) = this.getBoundingBox
        val shiftx = -lx1
        val shifty = -uy1
        val scaleX = xMax/(ux1 - lx1  + 1.0)
        val scaleY = yMax/(uy1 - ly1 + 1.0)
        listOfObjects.foreach(f => f.render(g,scaleX, -scaleY, shiftx, shifty))
    }

    def overlap(c2: MyCanvas): MyCanvas = {
        new MyCanvas(listOfObjects ++ c2.listOfObjects)
    }

    // DO NOT EDIT THE CODE BELOW
    override def toString: String = {
        listOfObjects.foldLeft[String] ("") { case (acc, fig) => acc ++ fig.toString }
    }
    // DO NOT EDIT
    def getListOfObjects: List[Figure] = listOfObjects

    // DO NOT EDIT
    def numPolygons: Int =
        listOfObjects.count {
            case Polygon(_) => true
            case _ => false }

    //DO NOT EDIT
    def numCircles: Int = {
        listOfObjects.count {
            case MyCircle(_,_) => true
            case _ => false }
    }
    //DO NOT EDIT
    def numVerticesTotal: Int = {
        listOfObjects.foldLeft[Int](0) ((acc, f) =>
            f match {
                case Polygon(lst1) => acc + lst1.length
                case _ => acc
            }
        )
    }
}
