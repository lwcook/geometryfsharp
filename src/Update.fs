namespace Update

open Geometry

// MODEL
type Model = {
  Total: int;
  Bezier: QuadraticBezierType;
  Coords: float * float;
  TextInput: string;
}

type Msg =
| SetInput of t: string
| Increment 
| Decrement 
| Reset
| MouseMove of x: float * y: float
| MouseDown of x: float * y: float


module Update = 

    let init() : Model =

      // As we'll see later, myCanvas is mutable hence the use of the mutable keyword
      // the unbox keyword allows to make an unsafe cast. Here we assume that getElementById will return an HTMLCanvasElement 
      // let window = Browser.Dom.window
      // let mutable myCanvas : Browser.Types.HTMLCanvasElement = unbox window.document.getElementById "myCanvas"  // myCanvas is defined in public/index.html

      {
        Total=0;
        Bezier={Control1=(10., 400.); Control2=(10., 10.); Control3=(410., 10.)};
        Coords=(0., 0.);
        TextInput="nothing";
      }

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
        | SetInput(t) -> {model with TextInput = t}
