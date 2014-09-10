---
title: Harris corner detector
description: Implementation of the harris corner detection algorithm using Haskell.
tags: cv, Repa, draft
mathjax: on
---

__TODO__ SSD & interest point

![Original image of some Paris building.](/images/harris-orig.bmp)

SSD(corner metric):

\\[ E(u, v) = \\sum_{x, y} w(x, y) (I(x + u, y + v) - I(x, y))^2 \\]

Taylor expansion:

\\[ I(x + u, y + v) = I(x, y) + u I_x(x, y) + v I_y(x, y) \\]

Substitution and cancelling:

\\[ E(u, v) = \\sum_{x, y} w(x, y) (u I_x(x, y) + v I_y(x, y))^2 \\]

Decompose dot product:

$$ E(u, v) = \\sum_{x, y} w(x, y)
   (\begin{bmatrix}u & v\end{bmatrix} \begin{bmatrix}I_x(x, y) \\ I_y(x, y)\end{bmatrix})^2
$$

Expand square and omit parens(associativity of product):

$$ E(u, v) = \\sum_{x, y} w(x, y)
   \begin{bmatrix}u & v\end{bmatrix} \begin{bmatrix}I_x(x, y) \\ I_y(x, y)\end{bmatrix}
   \begin{bmatrix}u & v\end{bmatrix} \begin{bmatrix}I_x(x, y) \\ I_y(x, y)\end{bmatrix}
$$

The "transpose of product" rule:

$$ E(u, v) = \\sum_{x, y} w(x, y)
   \begin{bmatrix}u & v\end{bmatrix} \begin{bmatrix}I_x(x, y) \\ I_y(x, y)\end{bmatrix}
   \begin{bmatrix}I_x(x, y) && I_y(x, y)\end{bmatrix}\begin{bmatrix}u \\ v\end{bmatrix}
$$

Find product:

$$ E(u, v) = \\sum_{x, y} w(x, y)
   \begin{bmatrix}u & v\end{bmatrix}
   \begin{bmatrix}I_x(x, y) I_x(x, y) && I_x(x, y) I_y(x, y) \\ I_y(x, y) I_x(x, y) && I_y(x, y) I_y(x, y)\end{bmatrix}
   \begin{bmatrix}u \\ v\end{bmatrix}
$$

Float in sum (uv vector does not depend on sum) and give a name for the covariance matrix:

$$ E(u, v) = \begin{bmatrix}u & v\end{bmatrix} M \begin{bmatrix}u \\ v\end{bmatrix} $$

$$ M = \\sum_{x, y} w(x, y) \begin{bmatrix}I_x(x, y) I_x(x, y) && I_x(x, y) I_y(x, y) \\ I_y(x, y) I_x(x, y) && I_y(x, y) I_y(x, y)\end{bmatrix} $$

__TODO__

* both \\lambda_1 and \\lambda_2 are small -- flat region;
* $\\lambda_1 >> \\lambda_2 $or \\lambda_1 >> \\lambda_2 -- edge region;
* both \\lambda_1 and \\lambda_2 are large -- corner region.

Using this relations for eigenvalues:

\\[ det(M) = \\lambda_1 * \\lambda_2 \\]

\\[ trace(M) = \\lambda_1 + \\lambda_2 \\]

Response:

\\[ R = det(M) - k * trace(M) ^ 2 \\]

> module Main where
> import Data.Array.Repa as R
> import Data.Array.Repa.IO.BMP as IO
> import Data.Array.Repa.Algorithms.Pixel as R
> import Data.Array.Repa.Algorithms.Convolve as R
> import Data.List as L
> import Data.Vector.Unboxed.Base as V
> import Data.Word
> import System.Environment
>
> type Image  = Array U DIM2
>
> harrisP :: Int              -- ^ size of gaussian;
>         -> Float            -- ^ harris free parameter;
>         -> Image Float      -- ^ input image;
>         -> IO (Image Float) -- ^ output image.
> harrisP s k i = do
>     i_x   <- convolveOutP outClamp sobelX i :: IO (Image Float)
>     i_y   <- convolveOutP outClamp sobelY i :: IO (Image Float)
>     i_xx  <- computeP $ R.zipWith (*) i_x i_x
>     i_xxw <- convolveOutP outClamp (gaussKern2d s) i_xx
>     i_xy  <- computeP $ R.zipWith (*) i_x i_y
>     i_xyw <- convolveOutP outClamp (gaussKern2d s) i_xy
>     i_yy  <- computeP $ R.zipWith (*) i_y i_y
>     i_yyw <- convolveOutP outClamp (gaussKern2d s) i_yy
>     computeP $ Main.zipWith3 response i_xxw i_xyw i_yyw
>   where
>     response xx xy yy = (xx * yy - xy * xy) - k * ((xx + yy) ^ 2)

Result
----------

![Image corners highlighted with red dots. To get the final image
non-maximum suppression and naive thresholding is
used.](/images/harris-corners.bmp)

Some considerations to make the method more robust:

* output of Harris detector __must__ be fed to non-maximum suppression
to get precise corner locations;

* histogram equalization may be applied to response map in order to
keep number of corners constant.

* __TODO__ Otsu's method?

Further reading
----------

[A combined corner and edge detector][harris-stephens]. C. Harris and
M. Stephens (1988). Proceedings of the 4th Alvey Vision
Conference. pp. 147–151.

[harris-stephens]: http://www.bmva.org/bmvc/1988/avc-88-023.pdf

Appendix
----------
Main function used to generate image samples and helper functions.

> type Kernel = Array U DIM2
>
> sobelX :: Unbox a => Num a => Kernel a
> sobelX = R.fromListUnboxed (Z :. 3 :. 3)
>   [ -1, 0, 1
>   , -2, 0, 2
>   , -1, 0, 1
>   ]
>
> sobelY :: Unbox a => Num a => Kernel a
> sobelY = R.fromListUnboxed (Z :. 3 :. 3)
>   [  1,  2,  1
>   ,  0,  0,  0
>   , -1, -2, -1
>   ]
>
> -- | Gaussian function.
> gaussian2d :: Floating a => a -> DIM2 -> a
> gaussian2d sigma ix @ (Z :. xi :. yi)  = coeff * exp (negate degree)
>   where
>     coeff  = recip (2 * pi * sigma * sigma)
>     degree = (x * x + y * y) / (2 * sigma * sigma)
>
>     x = fromIntegral xi
>     y = fromIntegral yi
>
> -- | Gaussian kernel.
> gaussKern2d :: Unbox a => Floating a => Int -> Kernel a
> gaussKern2d s = computeS $ R.fromFunction (Z :. s :. s) sample
>   where
>     cs = s `div` 2
>     sample (Z :. x :. y) = gaussian2d (fromIntegral (s * s) / 36) (Z :. (x - cs) :. (y - cs))
>
> zipWith3 :: (Source r1 a, Source r2 b, Source r3 c) => (Shape sh)
>          => (a -> b -> c -> d)
>          -> Array r1 sh a -> Array r2 sh b -> Array r3 sh c -> Array D sh d
> zipWith3 f a b c = traverse3 a b c (\ sh _ _ -> sh) produce where
>   produce getA getB getC pos = f (getA pos) (getB pos) (getC pos)
>
> -- | Non-maximum suppression.
> nms :: Image Float -> Image Float
> nms img = computeS  $ R.zipWith suppress extrema img
>  where
>    suppress e x = if x == e then x else 0
>    extrema = R.foldS max 0 $ R.foldS max 0 neighborhood
>    neighborhood = R.backpermute (extent img :. 3 :. 3) f img
>      where
>        f (Z :. y :. x :. j :. i) = Z :. posX :. posY
>          where
>            posX = min (w - 1) $ max 0 (y + j - 1)
>            posY = min (h - 1) $ max 0 (x + i - 1)
>            Z :. h :. w = extent img
>
> range :: Image Float -> (Float, Float)
> range i = (foldAllS min (1/0) i, foldAllS max (-1/0) i)
>
> normalize :: Image Float -- ^ arbitrary range array;
>           -> Image Float -- ^ each value of the array scaled to [0..1] range;
> normalize i = computeS $ R.map cv i
>   where
>     cv x = ((x - mn) * rr)
>     rr = recip (mx - mn)
>     (mn, mx) = range i
>
> -- | Apply a single threshold to an array.
> quantizeS :: (Source r a, Shape sh)
>           => (Ord a)
>           => a -- ^ threshold;
>           -> Array r sh a -> Array D sh Bool
> quantizeS t = R.map (\ x -> x >= t)
>
> -- | Morphological erosion.
> erode :: DIM2 -- ^ size of structuring element;
>       -> Image Bool -> IO (Image Bool)
> erode sh i = fmap (computeS . R.map (>= 1))
>            $ convolveOutP outClamp k
>            $ computeS $ R.map fromEnum i
>   where
>     k = R.computeS $ R.fromFunction sh (const 1)
>
> type RGB = (Word8, Word8, Word8)
>
> blend :: Image Bool -> Image RGB -> Image RGB -> Image RGB
> blend maskArr aArr bArr = computeS $ Main.zipWith3 mix maskArr aArr bArr
>   where
>     mix mask a b
>       |    mask   = b
>       | otherwise = a
>
> redImage :: DIM2 -> Image RGB
> redImage sh = R.computeS $ R.fromFunction sh (const (255, 0, 0))
>
> main :: IO ()
> main = do
>   args <- getArgs
>   let (src, dst, s, k, t) = case args of {
>     [srcStr, dstStr, sStr, kStr, tStr] -> (srcStr, dstStr, read sStr, read kStr, read tStr);
>     _      -> ("images/harris-orig.bmp", "images/harris-corners.bmp", 7, 0.04, 0.5)
>     }
>
>   putStrLn "Loading original image..."
>   Right orig <- readImageFromBMP src
>
>   putStrLn "Color to grayscale convertion..."
>   gray <- computeP $ R.map floatLuminanceOfRGB8 orig :: IO (Image Float)
>
>   putStrLn "Corner detection..."
>   responses <- harrisP s k gray
>   let corners = quantizeS t $ nms $ normalize responses
>   let npoints = L.head $ R.toList $ sumS $ sumS $ R.map fromEnum corners
>   putStrLn $ "Corners detected: " L.++ show npoints
>
>   putStrLn "Composing final image..."
>   mask <- erode (Z :. 3 :. 3) $ computeS corners
>   writeImageToBMP dst $ blend mask orig (redImage (extent orig))

This is a Literate Haskell file, so you can clone and run it using the
"[Source](#source)" link below. The accompanied source code is not
performance-wise, have not been optimized for performance and/or
memory usage and written for demostration purposes only.

It is assumed you have [Haskell Platform][haskell-platform]
installed. In order to run the code snippets you need also to install
[repa][hackage-repa], [repa-algorithms][hackage-repa-algorithms],
[repa-io][hackage-repa-io] packages. This can be done by issuing
`$ cabal install repa repa-algorithms repa-io` command in your
terminal. Build it with `$ ghc -O3 -Odph -optlo-O3 -threaded -llvm`
flags for better performance.

Usage: `harris input.bmp output.bmp $GAUSS_SIZE $HARRIS_FREE_COEFF $THRESHOLD`

[haskell-platform]:        https://www.haskell.org/platform/
[hackage-repa]:            http://hackage.haskell.org/package/repa
[hackage-repa-algorithms]: http://hackage.haskell.org/package/repa-algorithms
[hackage-repa-io]:         http://hackage.haskell.org/package/repa-io
