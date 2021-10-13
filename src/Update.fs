namespace Update

open Geometry

type Model = {
  Total: int;
  Coords: float * float;
  TextInput: string;
  Shapes: Geometry.Shape list;
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
      {
        Total=0;
        Coords=(0., 0.);
        TextInput="nothing";
        Shapes=[QuadraticBezier({Control1=(10., 400.); Control2=(10., 10.); Control3=(410., 10.)})];
      }
     
    let updateControlPoint (bezier: QuadraticBezierType) (point: Point2dType) =
          let indexedDists = List.mapi (fun i p -> (i, Point2d.distance point p)) <| QuadraticBezier.listControlPoints bezier
          let minIndex = List.minBy (fun (i, p) -> p) indexedDists
          let newBezier = match minIndex with 
                          | (0, _) -> {bezier with Control1=point}
                          | (1, _) -> {bezier with Control2=point}
                          | (2, _) -> {bezier with Control3=point}
                          | (_, _) -> bezier
          Geometry.QuadraticBezier(newBezier)
    
    let updateShapeFromMouse (shape: Shape) (mouse: Point2dType) =
        match shape with
        | QuadraticBezier(bezier) -> updateControlPoint bezier mouse
        | Circle(circle) -> Circle(circle)

    let updateFromMouse (model: Model) (mouse: Point2dType) : Model =
        {model with Coords = mouse; Shapes = [updateShapeFromMouse (model.Shapes.Head) mouse]}

    let update (msg:Msg) (model:Model) =
        match msg with
        | Increment -> {model with Total = model.Total + 1}
        | Decrement -> {model with Total = model.Total - 1}
        | Reset -> {model with Total = 0}
        | MouseMove(x, y) -> {model with Coords = (x, y)}
        | MouseDown(x, y) -> (updateFromMouse model (x, y))
        | SetInput(t) -> {model with TextInput = t}
