module App

(**
 The famous Increment/Decrement ported from Elm.
 You can find more info about Elmish architecture and samples at https://elmish.github.io/
*)

open Elmish

open Fable.React
open Fable.React.Props
open Fable.React.DrawingCanvas

open Browser.Types

open Geometry

// TODO: Work out how to do this without mutable
let mutable containerE1: Element = null
let mutable containerE2: Element = null

let toClientXY (e: MouseEvent) :float * float =
  let r = containerE1.getBoundingClientRect ()
  (e.clientX - r.left, e.clientY - r.top)
  //(e.clientX, e.clientY)

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

let bezierPoints (bezier: QuadraticBezierType) =
  let flip f a b = f b a
  (flip List.map [0. .. 0.01 .. 1.01] <| QuadraticBezier.alongCurve bezier)

// MODEL
type Model = {
  Total: int;
  Bezier: QuadraticBezierType;
  Coords: float * float;
}

type Msg =
| Increment 
| Decrement 
| Reset
| MouseMove of x: float * y: float
| MouseDown of x: float * y: float

let init() : Model =

  // As we'll see later, myCanvas is mutable hence the use of the mutable keyword
  // the unbox keyword allows to make an unsafe cast. Here we assume that getElementById will return an HTMLCanvasElement 
  // let window = Browser.Dom.window
  // let mutable myCanvas : Browser.Types.HTMLCanvasElement = unbox window.document.getElementById "myCanvas"  // myCanvas is defined in public/index.html

  {
    Total=0;
    Bezier={Control1=(10., 400.); Control2=(10., 10.); Control3=(410., 10.)};
    Coords=(0., 0.);
  }

// UPDATE

let updateControlPoint (model: Model) (mouse: Point2dType) : Model =
    let indexedDists = List.mapi (fun i p -> (i, Point2d.distance mouse p)) <| QuadraticBezier.listControlPoints model.Bezier
    let minIndex = List.minBy (fun (i, p) -> p) indexedDists
    printf "mouse %s" (string (mouse))
    printf "Bezier %s" (string (model.Bezier))
    printf "minimumIndex %s" (string (fst minIndex))
    printf "indexedDists %s" (string (indexedDists))
    let newBezier = 
      match minIndex with 
      | (0, _) -> {model.Bezier with Control1=mouse}
      | (1, _) -> {model.Bezier with Control2=mouse}
      | (2, _) -> {model.Bezier with Control3=mouse}
      | (_, _) -> model.Bezier

    {model with Coords = mouse; Bezier = newBezier}

let update (msg:Msg) (model:Model) =
    match msg with
    | Increment -> {model with Total = model.Total + 1}
    | Decrement -> {model with Total = model.Total - 1}
    | Reset -> {model with Total = 0}
    | MouseMove(x, y) -> {model with Coords = (x, y)}
    | MouseDown(x, y) -> (updateControlPoint model (x, y))

// VIEW (rendered with React)

let radius = 150.
let size = radius + 10.
let angle n = n * System.Math.PI

 
let view (model:Model) dispatch =

  let viewCoords coords :string =
    let x, y = coords
    String.concat ", " [string x; string y]

  let bezierpoints = bezierPoints model.Bezier
  let canvas = drawingcanvas { 
              Props = [ 
                OnMouseMove( fun e -> toClientXY (e) |> MouseMove |> dispatch )
                OnMouseDown( fun e -> toClientXY (e) |> MouseDown |> dispatch)
                Style [Width "500px"; Height "500px"]
                ] ; 
              Redraw =  redrawCanvas >>
                        drawPointsOnCanvas bezierpoints >> 
                        drawControlPoint model.Bezier.Control1 >>
                        drawControlPoint model.Bezier.Control2 >>
                        drawControlPoint model.Bezier.Control3 >>
                        fun _ -> () 
                        |> DrawFunction
            }

  // let rect = canvas.getBoundingClientRect ()
  // let elem: ReactElement = 0
  // let rect = Fable.React.ReactElement.getBoundingClientRect 

  div []
      [ 
        div [ Ref(fun e -> containerE1 <- e); Style [Width "500px"; Height "500px"]] [ canvas ]
        // button [ Props.OnClick (fun _ -> dispatch Increment) ] [ str "+" ]
        // div [] [ str (string model.Total) ]
        // button [ Props.OnClick (fun _ -> dispatch Decrement) ] [ str "-" ] 
        // div [ Ref(fun e -> containerE2 <- e) ] [ str (viewCoords model.Coords) ]
        // div [] [ str (viewCoords bezierpoints.Head) ]
        // div [] []
        // button [ OnClick (fun _ -> dispatch Reset) ] [ str "reset" ] 
        // div [] []
        // svg [ Style [ Border "1px solid green"; Width (snd model.xlims); Height (snd model.ylims )] ] [
            // circle [ 
              // R 60
              // Fable.React.Props.SVGAttr.Fill "green" 
              // Fable.React.Props.SVGAttr.Cx 150
              // Fable.React.Props.SVGAttr.Cy 100
            // ] []
            // circle [ 
              // R 60
              // Fable.React.Props.SVGAttr.Fill "red" 
              // Fable.React.Props.SVGAttr.Cx 350
              // Fable.React.Props.SVGAttr.Cy 100
            // ] []
            // polyline [ Points <| pointsToPolyLine myPoints; Fable.React.Props.Stroke "black" ] []
        // ]
      ] // App

Program.mkSimple init update view
|> React.Program.withReactSynchronous "elmish-app"
|> Program.withConsoleTrace
|> Program.run
