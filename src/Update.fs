namespace Update

open Geometry

type InteractionMode =
    | Hovering
    | EditingShape

type Model = {
    Mode: InteractionMode;
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
| MouseUp 


module Update = 

    let shapesFromText (t: string) : Shape list =
        let splitString = t.Split('\n') |> Array.toList
        let readShapeFromLine (s: string) :Shape list =
            match s with 
            | txt when txt.StartsWith("Circle") -> [Circle({Radius=20.; Centre=(100., 100.)})]
            | txt when txt.StartsWith("Curve") -> [ QuadraticBezier({Control1=(10., 400.); Control2=(10., 10.); Control3=(410., 10.)}) ]
            | _ -> []
        List.reduce List.append (List.map readShapeFromLine splitString)
        // split = String.

    let init() : Model =
        let initialText = "Circle\nCircle"
        // let exampleShapes = [
            //   Circle({Radius=20.; Centre=(100., 100.)})
            //   QuadraticBezier({Control1=(20., 300.); Control2=(50., 50.); Control3=(210., 5.)})
        //   ]
        {
          Mode=Hovering;
          Total=0;
          Coords=(0., 0.);
          TextInput=initialText;
          Shapes=shapesFromText initialText;
        }
    
    let updateControlPoint (bezier: QuadraticBezierType) (point: Point2dType) =
        let threshold = 10. 
        let indexedDists = List.mapi (fun i p -> (i, Point2d.distance point p)) <| QuadraticBezier.listControlPoints bezier
        let minIndex = List.minBy (fun (_, p) -> p) indexedDists
        let _, dist = minIndex
        if dist < threshold then
            let newBezier = match minIndex with 
                            | (0, _) -> {bezier with Control1=point}
                            | (1, _) -> {bezier with Control2=point}
                            | (2, _) -> {bezier with Control3=point}
                            | (_, _) -> bezier
            Geometry.QuadraticBezier(newBezier)
        else
            Geometry.QuadraticBezier(bezier)
    

    let updateCircleCenter (c: CircleType) (point: Point2dType) = 
        let threshold = 10.
        let dist = Point2d.distance point c.Centre
        if dist < threshold then
            Circle({c with Centre = point})
        else
            Circle(c)
    
    let updateShapeFromMouse (shape: Shape) (mouse: Point2dType) =
        match shape with
        | QuadraticBezier(bezier) -> updateControlPoint bezier mouse
        | Circle(circle) -> updateCircleCenter circle mouse

    let updateFromMovement (model: Model) (mouse: Point2dType) : Model =
        let flip f a b = f b a
        match model.Mode with
            | Hovering -> model
            | EditingShape -> {model with Coords = mouse; Shapes = List.map (flip updateShapeFromMouse mouse) model.Shapes}
    

    let update (msg:Msg) (model:Model) =
        match msg with
        | Increment -> {model with Total = model.Total + 1}
        | Decrement -> {model with Total = model.Total - 1}
        | Reset -> {model with Total = 0}
        | MouseMove(x, y) -> (updateFromMovement model (x, y))
        | MouseDown(x, y) -> {model with Coords = (x, y); Mode = EditingShape}
        | MouseUp -> {model with Mode = Hovering}
        | SetInput(t) -> {model with TextInput = t; Shapes = shapesFromText t}
