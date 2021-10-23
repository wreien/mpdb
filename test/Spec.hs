module Main where

import Test.Hspec
import Main.Utf8 (withUtf8)

import LexerSpec

main :: IO ()
main = withUtf8 $ hspec $ do
  LexerSpec.spec
