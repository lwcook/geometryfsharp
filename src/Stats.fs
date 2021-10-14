namespace Stats

open System

module MultivariateNormal =

    let rec innerMult u v =
        match u, v with 
        | [x], [y] -> x*y     
        | u'::u, v'::v -> u'*v' + innerMult u v
        | _, [] -> 0.
        | [], _ -> 0.
    

    let rec transpose m = 
        match m with
        | (_::_)::_ as m' -> List.map List.head m' :: transpose (List.map List.tail m')
        | _ -> []
     
    let scalarMult (x: float) (m: float list list) =
        let rowMult r = 
            List.map (fun y -> x * y) r
        List.map rowMult m
    
    let matrixMult xs ys =
        let yst = transpose ys
        let rec innerRowMult xs' ys' =
            match xs', ys' with 
            | r :: rs, t :: ts -> innerMult r t :: innerRowMult rs ts
            | _, [] -> []  // Allows vector * matrix
            | [], _ -> []  // Allows matrix * vector
        [innerRowMult xs yst]

    let determinant2D (xs: float list list) = 
        let a, b = xs.[0].[0], xs.[0].[1]
        let c, d = xs.[1].[0], xs.[1].[1]
        a * d - b * c

    let inverse2D (xs: float list list) = 
        let a, b = xs.[0].[0], xs.[0].[1]
        let c, d = xs.[1].[0], xs.[1].[1]
        scalarMult (1. / (determinant2D xs)) [[d; -b]; [-c; a]]

    let sub (u: float list list) (v: float list list) = 
        let subRow (ru, rv) = 
            List.map (fun (ui, vi) -> ui - vi) <| List.zip ru rv
        List.map subRow <| List.zip u v

    let add (u: float list list) (v: float list list) = 
        let subRow (ru, rv) = 
            List.map (fun (ui, vi) -> ui + vi) <| List.zip ru rv
        List.map subRow <| List.zip u v


    // Expecting mu and std to be column vectors like [[1]; [2]]
    let distribution2D (mu: float list list) (std: float list list) (x: float list list) = 
        let pi, cos, sin = Math.PI, Math.Sin, Math.Cos
        let d = List.length mu
        let first = matrixMult (transpose (sub x mu)) (inverse2D std)
        let second = sub x mu
        let exponent = (fun (x: float list list) -> x.[0].[0]) <| scalarMult (- 0.5) (matrixMult first second)
        (2. * pi) ** (-float d) * (Math.Sqrt <| determinant2D std) * (Math.Exp exponent)
     