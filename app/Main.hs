{-# LANGUAGE TemplateHaskell #-}

module Main where

import System.Environment (getArgs)

import Data.List.Split (splitOn)

import GLL.Combinators.Interface

import Language.Haskell.TH (Q, Exp, runQ)
import Language.Haskell.TH.Ppr (pprint)

import Mauke

simpleLexer :: String -> [Token]
simpleLexer = map Char

testParse :: SymbExpr Token [String]
testParse = "TestPlus" <::=> $(mkParserOp . lexOpStr $ "_+_if_blah")
                       <||>  $(mkParserOp . lexOpStr $ "_ _ _ ")

getHS :: Q Exp -> IO ()
getHS exp = let removeStr pat = concat . splitOn pat
                patterns      = [ "GLL.Combinators.Interface."
                                , "GHC.Types."
                                ]
                removeQualif  = foldr (.) id $ map removeStr patterns
            in  runQ exp >>= putStrLn . removeQualif . pprint

main :: IO ()
main = getArgs >>= putStrLn . show . parse testParse . simpleLexer . concat
