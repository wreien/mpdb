{-# LANGUAGE OverloadedStrings #-}

module Main where

import Main.Utf8 (withUtf8)
import Lexer
import Text.Megaparsec (runParser, errorBundlePretty)
import Prettyprinter
import Prettyprinter.Render.Text

import qualified Data.Text.IO as T

main :: IO ()
main = withUtf8 $ do
  file <- T.getContents
  let result = runParser lexer "stdin" file
  case result of
    Left e -> putStr $ errorBundlePretty e
    Right ts -> do
      putStrLn "Success:"
      T.putStr . renderStrict . layoutSmart defaultLayoutOptions . pretty $ ts
