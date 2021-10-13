namespace Geometry

type Point2dType = float * float

module Point2d =

    let create x y = (x, y)

    let add2 (p: Point2dType) (q: Point2dType) = 
        let (x1, y1) = p
        let (x2, y2) = q
        (x1 + x2, y1 + y2)

    let sub2 (p: Point2dType) (q: Point2dType) = 
        let (x1, y1) = p
        let (x2, y2) = q
        (x1 - x2, y1 - y2)


    let mult2 (a: float) (q: Point2dType) = 
        let (x1, y1) = q
        (a*x1, a*y1)

    let rint (x: float): int = int <| System.Math.Round x

    let distance (p: Point2dType) (q: Point2dType) : float =
        let (x1, y1) = p
        let (x2, y2) = q
        sqrt <| (x2 - x1)**2. + (y2 - y1)**2.
   
type QuadraticBezierType = {Control1: Point2dType; Control2: Point2dType; Control3: Point2dType}

module QuadraticBezier =

    let alongCurve (b: QuadraticBezierType) (t: float) :Point2dType =
        b.Control2 |> 
        Point2d.add2 (Point2d.mult2 ((1. - t)*(1. - t)) (Point2d.sub2 b.Control1 b.Control2)) |> 
        Point2d.add2 (Point2d.mult2 (t * t) (Point2d.sub2 b.Control3 b.Control2))
    
    let listControlPoints (b: QuadraticBezierType) =  
      [b.Control1; b.Control2; b.Control3]


type CircleType = {Radius: float; Centre: Point2dType}

module Circle = 

    let alongCurve (c: CircleType) (t: float) :Point2dType =
        let (x0, y0) = c.Centre
        (x0 + System.Math.Cos (2. * System.Math.PI * t), y0 + System.Math.Sin (2. * System.Math.PI * t))
 
type Shape = 
  | Circle of CircleType
  | QuadraticBezier of QuadraticBezierType