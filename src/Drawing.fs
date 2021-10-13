namespace Drawing

open Geometry

module Drawing =

    let redrawCanvas (ctx: Browser.Types.CanvasRenderingContext2D) =
        ctx.canvas.width <- ctx.canvas.offsetWidth
        ctx.canvas.height <- ctx.canvas.offsetHeight
        ctx.clearRect(0.0, 0.0, ctx.canvas.width, ctx.canvas.height)
        ctx


    let drawPointsOnCanvas (points: Point2dType list) (ctx: Browser.Types.CanvasRenderingContext2D) =
        ctx.save()
        ctx.lineWidth <- 0.5
        ctx.beginPath()
        ctx.stroke()
        ctx.restore()
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
        ctx.restore()
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