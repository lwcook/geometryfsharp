namespace Drawing

open Geometry

module Drawing =

    let bezierPoints (bezier: QuadraticBezierType) =
      let flip f a b = f b a
      (flip List.map [0. .. 0.01 .. 1.01] <| QuadraticBezier.alongCurve bezier)


    let redrawCanvas (ctx: Browser.Types.CanvasRenderingContext2D) =
        ctx.canvas.width <- ctx.canvas.offsetWidth
        ctx.canvas.height <- ctx.canvas.offsetHeight
        ctx.clearRect(0.0, 0.0, ctx.canvas.width, ctx.canvas.height)
        ctx

    let drawPointsOnCanvas (points: Point2dType list) (ctx: Browser.Types.CanvasRenderingContext2D) =
        ctx.lineWidth <- 0.5
        ctx.beginPath()
        ctx.stroke()
        let rec recurse ps = 
          match ps with 
            | [] ->  ()
            | p::tail ->  if List.isEmpty tail then ()
                          else 
                            let (x, y) = p
                            let (x2, y2) = List.head tail
                            ctx.moveTo(x, y)
                            ctx.lineTo(x2, y2)
                            recurse tail
        recurse points
        ctx.stroke()
        ctx.closePath()
        ctx

    let drawControlPoint (p: Point2dType) (ctx: Browser.Types.CanvasRenderingContext2D) =
        let r = 5.
        ctx.save()
        ctx.lineWidth <- 0.5
        ctx.beginPath()
        let (x, y) = Point2d.add2 p (0., 0.)  // offset
        ctx.arc(x, y, r, 0.0, System.Math.PI * 2.0, true)
        ctx.stroke()
        ctx.closePath()
        ctx

    let drawCircle (c: CircleType) (ctx: Browser.Types.CanvasRenderingContext2D) =
        ctx.save()
        let (x0, y0) = Point2d.add2 c.Centre (0., 0.)
        ctx.lineWidth <- 0.5
        ctx.beginPath()
        ctx.arc(x0, y0, c.Radius, 0.0, System.Math.PI * 2.0, true)
        ctx.stroke()
        ctx.closePath()
        ctx
     
    let drawBezier (b: QuadraticBezierType)  = 
        drawPointsOnCanvas (bezierPoints b) >> 
        drawControlPoint b.Control1 >>
        drawControlPoint b.Control2 >>
        drawControlPoint b.Control3
    
    let drawShape (s: Shape) =
        match s with 
        | QuadraticBezier(bezier) -> drawBezier bezier
        | Circle(circle) -> drawCircle circle
     
    let drawShapes (ss: Shape list) (ctx: Browser.Types.CanvasRenderingContext2D) :Browser.Types.CanvasRenderingContext2D =
        match ss with 
        | [] -> ctx
        | _ -> List.reduce (>>) (List.map drawShape ss) <| ctx  // Not sure why List.reduce fails on empty list