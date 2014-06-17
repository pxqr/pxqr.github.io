#!/usr/bin/env runhaskell
{-# LANGUAGE NoMonomorphismRestriction #-}
module Main (main) where
import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine

lambda :: Diagram B R2
lambda = fromVertices
  [ 0 ^& 0, 0.4 ^& 0.6
  , 0.2 ^& 1.0, 0.4 ^& 1.0
  , 1.0 ^& 0, 0.8 ^& 0
  , 0.5 ^& 0.5
  , 0.2 ^& 0, 0 ^& 0
  ]

main :: IO ()
main = mainWith $ fillColor gray lambda