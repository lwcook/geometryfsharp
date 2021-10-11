module App

(**
 The famous Increment/Decrement ported from Elm.
 You can find more info about Elmish architecture and samples at https://elmish.github.io/
*)

open Elmish
open Elmish.React
open Fable.React
open Fable.React.Props

// MODEL

let a = [1; 2; 3]
let b = (1, 2)

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

let view (model:Model) dispatch =

  div []
      [ 
        button [ OnClick (fun _ -> dispatch Increment) ] [ str "+" ]
        div [] [ str (string model.total) ]
        button [ OnClick (fun _ -> dispatch Decrement) ] [ str "-" ] 
        div [] []
        button [ OnClick (fun _ -> dispatch Reset) ] [ str "reset" ] 
        div [] []
        svg [ Style [ Border "1px solid green"; Width (snd model.xlims); Height (snd model.ylims )] ] [
            circle [ 
              R 60
              Fable.React.Props.SVGAttr.Fill "green" 
              Fable.React.Props.SVGAttr.Cx 150
              Fable.React.Props.SVGAttr.Cy 100
            ] []
            circle [ 
              R 60
              Fable.React.Props.SVGAttr.Fill "red" 
              Fable.React.Props.SVGAttr.Cx 350
              Fable.React.Props.SVGAttr.Cy 100
            ] []
            polyline [ Points "0,0 10,10, 20,20, 100,100"; Fable.React.Props.Stroke "black" ] []
        ]
      ] // App

Program.mkSimple init update view
|> Program.withReactSynchronous "elmish-app"
|> Program.withConsoleTrace
|> Program.run
