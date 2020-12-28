-- |
-- Module      :  Main
-- Copyright   :  (c) OleksandrZhabenko 2019-2020
-- License     :  MIT
-- Stability   :  Experimental
-- Maintainer  :  olexandr543@yahoo.com
--
-- An additional program that is used with the mmsyn7ukr executable as a sound creator with the voice
-- given by the files in the current directory. It is very similar to the Main.hs of the mmsyn6ukr package.

--

module Main where

import MMSyn7h 

{-
-- Inspired by: https://mail.haskell.org/pipermail/beginners/2011-October/008649.html
-}

-- | The main function. It calls 'main7h'.
main :: IO ()
main = do
  putStrLn "Next file can be now voiced by your \"voice\"."
  main7h

