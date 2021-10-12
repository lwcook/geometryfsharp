module App

(**
 The famous Increment/Decrement ported from Elm.
 You can find more info about Elmish architecture and samples at https://elmish.github.io/
*)

open Elmish
open Elmish.React
open Fable.React
open Fable.React.DrawingCanvas
open Fable.React.DrawingCanvas.Builder

open Geometry

let drawPointsOnCanvas (points: Point2dType list) (ctx: Browser.Types.CanvasRenderingContext2D) =
    ctx.save()
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


let pointsToPolyLine (points: Point2dType list) : string =

    let pToString (p: Point2dType) = 
      let (x, y) = p
      String.concat "," [string <| Point2d.rint x; string <| Point2d.rint y] 

    String.concat " " <| List.map pToString points

let myCurve = QuadraticBezier.alongCurve {Control1=(10., 100.); Control2=(10., 10.); Control3=(110., 10.)}
let myPoints = List.map myCurve [0. .. 0.05 .. 1.0]

// MODEL
type Model = {
  total : int;
  xlims : int * int;
  ylims : int * int;
}

type Msg =
| Increment | Decrement | Reset

let init() : Model =

  // As we'll see later, myCanvas is mutable hence the use of the mutable keyword
  // the unbox keyword allows to make an unsafe cast. Here we assume that getElementById will return an HTMLCanvasElement 
  // let window = Browser.Dom.window
  // let mutable myCanvas : Browser.Types.HTMLCanvasElement = unbox window.document.getElementById "myCanvas"  // myCanvas is defined in public/index.html

  {total=0; xlims=(0, 500); ylims=(0, 500)}

// UPDATE

let update (msg:Msg) (model:Model) =
    match msg with
    | Increment -> {total=0; xlims=(0, 500); ylims=(0, 500)}
    | Decrement -> {total=0; xlims=(0, 500); ylims=(0, 500)}
    | Reset -> {total=0; xlims=(0, 500); ylims=(0, 500)}

// VIEW (rendered with React)

let radius = 150.
let size = radius + 10.
let angle n = n * System.Math.PI

let outerBorder = preserve {
  lineWidth 6.
  beginPath
  arc 0. 0. radius 0. (angle 1.0) true
  stroke
}

let staticdrawing = 
  drawing { 
    resize (2. * size) (2. * size)
    translate size size
    fillColor "#555555"
    strokeColor "#555555"
    sub outerBorder  
  }

let view (model:Model) dispatch =

  div []
      [ 
        div [] [
          drawingcanvas { Props = [] ; Redraw = DrawFunction <| drawPointsOnCanvas myPoints }
        ]
        // button [ OnClick (fun _ -> dispatch Increment) ] [ str "+" ]
        // div [] [ str (string model.total) ]
        // button [ OnClick (fun _ -> dispatch Decrement) ] [ str "-" ] 
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
|> Program.withReactSynchronous "elmish-app"
|> Program.withConsoleTrace
|> Program.run
