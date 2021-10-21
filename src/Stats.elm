module Stats exposing (..)

import Matrix
import RowVector
import Vector
import InvertableMatrix
import SquareMatrix
import NormalMatrix
import ColumnVector
import RowVector
import Real
import InvertableMatrix exposing (InvertableMatrix)
import RowVector exposing (RowVector)
import ColumnVector exposing (ColumnVector)
import SquareMatrix exposing (SquareMatrix)
import Matrix exposing (Matrix)
import NormalMatrix exposing (NormalMatrix)


type alias RealFloat = Real.Real Float

type alias CovMat = InvertableMatrix.InvertableMatrix RealFloat

type alias ColVec = ColumnVector.ColumnVector RealFloat

type alias RowVec = RowVector.RowVector RealFloat


realVecSpace : RowVector.VectorSpace RealFloat
realVecSpace = RowVector.realVectorSpace

realDotSpace : RowVector.InnerProductSpace RealFloat
realDotSpace = RowVector.realInnerProductSpace

scalarMul : Float -> Matrix.Matrix RealFloat -> Matrix.Matrix RealFloat 
scalarMul a v = Matrix.scalarMultiplication Real.field (Real.Real a) v

calculateGaussianPDF : Matrix.Matrix RealFloat -> RealFloat -> Matrix.Matrix RealFloat -> Result String RealFloat
calculateGaussianPDF distanceVec (Real.Real determinantValue) inverse = 
    let 
        columnVectorDist = Matrix.transpose distanceVec
        first = Matrix.multiply realDotSpace (scalarMul -0.5 distanceVec) inverse
        (Matrix.Matrix rowVecs) = Matrix.multiply realDotSpace first columnVectorDist
    in
        case rowVecs of 
            [RowVector.RowVector (Vector.Vector [Real.Real val])] -> Ok <| Real.Real <| (1.0/(2.0 * pi)) * (1.0/ sqrt(determinantValue)) * (e ^ val)
            _ -> Err "Somehow the matrix wasn't a single element"


type alias GaussianDistribution = {
        mu: ColVec,
        cov: CovMat
    }

type alias TruncatedGaussianDistribution = {
        mu: ColVec,
        cov: CovMat,
        lims: (Float, Float)
    }


gaussianFromLists : List Float -> List (List Float) -> GaussianDistribution
gaussianFromLists mu cov = 
    let
        muVector = ColumnVector.ColumnVector <| Vector.Vector (List.map Real.Real mu)
        innerMap inner = RowVector.RowVector (Vector.Vector (List.map Real.Real inner))
        covMatrix = Matrix.Matrix <| List.map innerMap cov 
        covInvertible = InvertableMatrix.InvertableMatrix (NormalMatrix.NormalMatrix (SquareMatrix.SquareMatrix covMatrix))
    in {
        mu = muVector,
        cov = covInvertible
    }
        
    

normal2d : GaussianDistribution
normal2d = 
    let 
        row1 = RowVector.RowVector (Vector.Vector [Real.Real 1.0, Real.Real 0.0])
        row2 = RowVector.RowVector (Vector.Vector [Real.Real 0.0, Real.Real 1.0])
        aMatrix = Matrix.Matrix [row1, row2]
    in {
        mu = ColumnVector.ColumnVector <| Vector.Vector [Real.Real 0, Real.Real 0],
        cov = InvertableMatrix.InvertableMatrix (NormalMatrix.NormalMatrix (SquareMatrix.SquareMatrix aMatrix))
    }

safeGaussianPDF : GaussianDistribution -> ColumnVector.ColumnVector RealFloat -> Result String RealFloat
safeGaussianPDF {mu, cov} (ColumnVector.ColumnVector x) =
    let
        (ColumnVector.ColumnVector muv) = mu
        (InvertableMatrix.InvertableMatrix (NormalMatrix.NormalMatrix (SquareMatrix.SquareMatrix covMat))) = cov
        murow = Matrix.Matrix [RowVector.RowVector muv]
        xrow = Matrix.Matrix [RowVector.RowVector x]
        determinant = InvertableMatrix.determinant realVecSpace cov
        inverse = InvertableMatrix.invert realDotSpace cov
    in 
    case (determinant, inverse) of 
        (Ok det, Ok inv) -> 
            let 
                (InvertableMatrix.InvertableMatrix (NormalMatrix.NormalMatrix (SquareMatrix.SquareMatrix aMatrix))) = inv
                res = calculateGaussianPDF (Matrix.subtract Real.field xrow murow) det aMatrix
            in
            case res of 
                Ok ans -> Ok ans
                Err txt -> Err txt
        (Err txt, Ok _) -> Err txt
        (Ok _, Err txt) -> Err txt
        (Err txt1, Err txt2) -> Err (txt1 ++ txt2)
    

gaussianPDF : GaussianDistribution -> ColumnVector.ColumnVector RealFloat -> Float
gaussianPDF dist point =
    case safeGaussianPDF dist point of 
        Ok (Real.Real val) -> val
        Err _ -> 0.0



rejectionSampleGaussianTruncated : TruncatedGaussianDistribution -> Vector.Vector RealFloat 
rejectionSampleGaussianTruncated dist =
    let 
        randomSample = List.Map 
    in
    Vector.Vector <| List.Map Real.Real [0.0, 0.1]
    