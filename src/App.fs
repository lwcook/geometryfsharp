module App

(**
 The famous Increment/Decrement ported from Elm.
 You can find more info about Elmish architecture and samples at https://elmish.github.io/
*)

open Elmish

open Fable.React  // Some web components, div etc.
open Fable.React.Props  // Properties for web components, Style etc.
open Fable.React.DrawingCanvas

open Browser.Types

open Fulma  // More nice web components, Columns etc.

open Geometry
open Drawing

open Update

// TODO: Work out how to do this without mutable
let mutable containerE1: Element = null
let mutable containerE2: Element = null

let toClientXY (e: MouseEvent) :float * float =
  let r = containerE1.getBoundingClientRect ()
  (e.clientX - r.left, e.clientY - r.top)

let bezierPoints (bezier: QuadraticBezierType) =
  let flip f a b = f b a
  (flip List.map [0. .. 0.01 .. 1.01] <| QuadraticBezier.alongCurve bezier)

// VIEW (rendered with React)
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
              Redraw =  Drawing.redrawCanvas >>
                        Drawing.drawPointsOnCanvas bezierpoints >> 
                        Drawing.drawControlPoint model.Bezier.Control1 >>
                        Drawing.drawControlPoint model.Bezier.Control2 >>
                        Drawing.drawControlPoint model.Bezier.Control3 >>
                        fun _ -> () 
                        |> DrawFunction
            }

  Container.container [ Container.Props [ Style [ MarginTop "48px"] ] ] [
    Columns.columns [ Columns.IsCentered ] [
      Column.column [ Column.Width(Fulma.Screen.All, Column.Is6) ] [
        Heading.h5 [] [str "Hello Baby"] 
      ]
    ]
    Columns.columns [ Columns.IsCentered ] [
      Column.column [ Column.Width(Screen.All, Column.IsTwoFifths) ] [
        Textarea.textarea [ Textarea.Props [ 
          Style [Height "400px"; Width "400px"] 
          Value model.TextInput
          OnChange(fun e -> e.Value |> SetInput |> dispatch)
          ] 
        ] []
      ]
      Column.column [ Column.Width(Screen.All, Column.IsTwoFifths) ] [
        div [ Ref(fun e -> containerE1 <- e); Style [Width "500px"; Height "500px"]] [ canvas ]
      ]
    ]
  ]

Program.mkSimple Update.init Update.update view
|> React.Program.withReactSynchronous "elmish-app"
|> Program.withConsoleTrace
|> Program.run
