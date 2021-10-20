namespace Update

open Geometry

open FSharp.Stats

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

    let radiusDistribution = Distributions.Continuous.normal 50. 5.0 
    // let xDistribution = Distributions.Continuous.normal 100. 5.0 
    // let yDistribution = Distributions.Continuous.normal 100. 5.0 
    let defaultCircle = Circle({Radius=20.; Centre=(100., 100.)})

    let sampleCircles a b c : Shape list = 
        [defaultCircle]
        // let (rmu, rstd) = match a with 
                        //   | Some r -> (r, 0.0)
                        //   | None -> (50.0, 3.0)
        // let (xmu, xstd) = match b with 
                        //   | Some x -> (x, 0.0)
                        //   | None -> (100.0, 5.0)
        // let (ymu, ystd) = match c with 
                        //   | Some y -> (y, 0.0)
                        //   | None -> (100.0, 5.0)
        // let mean = vector [rmu; xmu; ymu]
        // let covar = matrix [[rstd;0.;0.];[0.;xstd;0.;0.;0.;ystd]]
        // let dist3d = Distributions.Continuous.multivariateNormal  mean covar
        // let samples = Array.init 20 (fun _ -> dist3d.Sample()) |> Array.toList
        // let toCircle (sample: vector) = 
            // Circle({Radius=sample.[0]; Centre=(sample.[1], sample.[2])})
        // List.map toCircle samples

    let circlesFromText (txt: string) : Shape list =
        let args = txt.Split(' ') |> Array.toList

        let parseChar c = 
            try
                Some <| float c
            with 
            | _ -> None

        match args with 
        | x :: tail ->  match tail with
                        | [a; b; c] -> sampleCircles (parseChar a) (parseChar b) (parseChar c)
                        | _ -> [defaultCircle]
        | _ -> [defaultCircle]
        // TODO: Could be one, or many samples


    let shapesFromText (t: string) : Shape list =
        let splitString = t.Split('\n') |> Array.toList
        let readShapeFromLine (s: string) :Shape list =
            match s with 
            | txt when txt.StartsWith("Circle") -> circlesFromText txt
            | txt when txt.StartsWith("Curve") -> [ QuadraticBezier({Control1=(10., 400.); Control2=(10., 10.); Control3=(410., 10.)}) ]
            | _ -> []
        List.reduce List.append (List.map readShapeFromLine splitString)


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
