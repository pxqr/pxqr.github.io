#!/usr/bin/env runhaskell
--
--  dependencies:
--
--   * diagrams-prelude
--   * diagrams-contrib
--   * diagrams-svg
--
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -Wall #-}
module Main (main) where
import Diagrams.Prelude
import Diagrams.TwoD.Grid
import Diagrams.Backend.SVG.CmdLine

diagram :: Diagram B R2
diagram = gridWithHalves 3 3

main :: IO ()
main = mainWith diagram