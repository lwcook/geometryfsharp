module Stats exposing (..)

import Matrix
import RowVector
import Vector
import InvertableMatrix
import SquareMatrix
import NormalMatrix
import RowVector
import Real
import Random
import InvertableMatrix exposing (InvertableMatrix)
import RowVector exposing (RowVector)
import ColumnVector exposing (ColumnVector)
import SquareMatrix exposing (SquareMatrix)
import Matrix exposing (Matrix)
import NormalMatrix exposing (NormalMatrix)


type alias RealFloat = Real.Real Float

type alias CovMat = InvertableMatrix.InvertableMatrix RealFloat

scalarMul : Float -> Matrix.Matrix RealFloat -> Matrix.Matrix RealFloat 
scalarMul a v = Matrix.scalarMultiplication Real.field (Real.Real a) v

calculateGaussianPDF : Matrix.Matrix RealFloat -> RealFloat -> Matrix.Matrix RealFloat -> Result String RealFloat
calculateGaussianPDF distanceVec (Real.Real determinantValue) inverse = 
    let 
        columnVectorDist = Matrix.transpose distanceVec
        first = Matrix.multiply  RowVector.realInnerProductSpace (scalarMul -0.5 distanceVec) inverse
        (Matrix.Matrix rowVecs) = Matrix.multiply RowVector.realInnerProductSpace first columnVectorDist
    in
        case rowVecs of 
            [RowVector.RowVector (Vector.Vector [Real.Real val])] -> Ok <| Real.Real <| (1.0/(2.0 * pi)) * (1.0/ sqrt(determinantValue)) * (e ^ val)
            _ -> Err "Somehow the matrix wasn't a single element"

type alias TruncatedGaussianDistribution = 
    { mu: Vector.Vector RealFloat
    , cov: InvertableMatrix.InvertableMatrix RealFloat
    , lims: (Float, Float)
    }
    

normal2d : TruncatedGaussianDistribution
normal2d = 
    let 
        row1 = RowVector.RowVector (Vector.Vector [Real.Real 1.0, Real.Real 0.0])
        row2 = RowVector.RowVector (Vector.Vector [Real.Real 0.0, Real.Real 1.0])
        aMatrix = Matrix.Matrix [row1, row2]
    in  { mu = Vector.Vector [Real.Real 0, Real.Real 0]
        , cov = InvertableMatrix.InvertableMatrix (NormalMatrix.NormalMatrix (SquareMatrix.SquareMatrix aMatrix)) 
        , lims = (-10.0, 10.0)
        }

safeGaussianPDF : TruncatedGaussianDistribution -> Vector.Vector RealFloat -> Result String RealFloat
safeGaussianPDF {mu, cov} x =
    let
        murow = Matrix.Matrix [RowVector.RowVector mu]
        xrow = Matrix.Matrix [RowVector.RowVector x]
        determinant = InvertableMatrix.determinant RowVector.realVectorSpace cov
        inverse = InvertableMatrix.invert RowVector.realInnerProductSpace cov
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
    

gaussianPDF : TruncatedGaussianDistribution -> Vector.Vector RealFloat -> Float
gaussianPDF dist point =
    case safeGaussianPDF dist point of 
        Ok (Real.Real val) -> val
        Err _ -> 0.0


generatorFromGaussian : TruncatedGaussianDistribution -> Int -> Random.Generator (List (List Float))
generatorFromGaussian dist numberSamples = vectorGenerator dist.mu dist.lims numberSamples

vectorGenerator : Vector.Vector a -> (Float, Float) -> Int -> Random.Generator (List (List Float))
vectorGenerator vector bounds numberSamples =
    let
        (Vector.Vector innerList) = vector
        number = List.length innerList
        (lower, upper) = bounds
    in 
    Random.list numberSamples <| Random.list number <| Random.float lower upper


rejectionSampleGaussianTruncated : TruncatedGaussianDistribution -> Vector.Vector RealFloat -> Vector.Vector RealFloat 
rejectionSampleGaussianTruncated dist randomSample =
    let 
        -- TODO: Gaussian sampling with cholesky decomposition?
        -- Rejection sampling is annoying, because we don't know how many samples we need to generate
        -- We will have to go back to the command many times
        a = 1
        -- P
        -- (ColumnVector.ColumnVector (Vector.Vector innerList)) = dist.mu
    in
    randomSample
    