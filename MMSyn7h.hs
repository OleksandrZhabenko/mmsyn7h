-- |
-- Module      :  MMSyn7h
-- Copyright   :  (c) OleksandrZhabenko 2019-2020
-- License     :  MIT
-- Stability   :  Experimental
-- Maintainer  :  olexandr543@yahoo.com
--
-- An additional program that is used with the mmsyn7ukr executable as a sound creator with the voice
-- given by the files in the current directory. It is very similar to the Main.hs of the mmsyn6ukr package.

--

{-# OPTIONS_GHC -threaded #-}

module MMSyn7h where

import Data.Char (isSpace, isControl)
import Data.Maybe (isJust, fromJust)
import System.IO
import System.Environment (getArgs)
import System.Process (readProcessWithExitCode)
import System.Directory (removeFile, listDirectory, getCurrentDirectory)
import Control.Exception (bracketOnError,onException)
import EndOfExe (showE)
import Melodics.Ukrainian (convertToProperUkrainian, takeData)
import UkrainianLControl
import qualified Data.Vector as V
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString as BS
import Data.List (isSuffixOf)
import CaseBi (getBFst')
import System.Info (os)
import MMSyn6Ukr.Show7s (show7s)
import Control.Exception.FinalException

-- | Function that proposes and creates if accepted the sound record with the new \"voice\". It plays the newly created file once. Then it can delete
-- the sound files in the current directory while being executed if you specify when prompted the text that starts with \"y\".
-- If you enter as a first command line argument \"-h\",
-- then the program only prints informational message. If you specify as a first command line argument \"-v\", then
-- the program only prints its version number. If you specify something else, the first command line argument is being treated as
-- a name for the resulting file voiced. If you specify further command line arguments
-- as a Ukrainian text, that contains only those sounds, which sound representations are in the current directory (you can create them by e. g. @mmsyn7ukr@ and @mmsyn7l@
-- programs in the same name packages), then the program will use only these sounds representations additionally to the default ones \"-.wav\",
-- \"0.wav\" and \"1.wav\" and produce the sounding for the text.
main7h :: IO ()
main7h = do
  args <- getArgs
  putStr "If you do not use the command line parameters \"-h\" or \"-v\", then you must have specified the file name for the resulting sound recording "
  putStrLn "(do NOT use '}' character and space or control characters!). "
  case (concat . take 1 $ args) of
    ""   -> catchEnd NotFileNameGiven
    "-h" -> do
          putStrLn "SYNOPSIS: "
          putStrLn "mmsyn7h fileName [control parameter (see: genControl from mmsyn6ukr package) [a Ukrainian text being one line to be voiced (if any specified)]]  OR"
          putStrLn "mmsyn7h -h    OR"
          putStrLn "mmsyn7h -v"
          putStr "If \"-h\" is specified, then you will see this message. If \"-v\" is specified, then you will see the version of the package mmsyn7h. "
          putStrLn "If something else (not null) is specified then the program runs further. "
    "-v" -> putStrLn "mmsyn7h version: 0.8.0.0"
    nameOfSoundFile    -> bracketOnError (do
          giveInfo
          let nameSF = filter (\x -> not (isSpace x) && not (isControl x) && x /= '}') nameOfSoundFile
          putStrLn ""
          return (args, nameSF)) (\(args, nameSF) -> do
            putStr "Notice, there was (may be) CmdLineArgument exception. To avoid it, please, specify the second command line argument (if needed) in the form \"ABC\""
            putStrLn $ " where A is either a letter \'f\', \'o\', \'w\' or a digit and B and C are both digits! The exception (may be) arose from the command line arguments "
              ++ show args ++ " for the file: " ++ show nameSF ++ ". Please, check also whether the SoX was installed with the support for needed codec.")
              (\(args, nameSF) -> do
                let arg = drop 1 . take 2 $ args
                if (not . null . drop 2 $ args)
                  then do
                    putStrLn ""
                    putStr "The resulting file will be played just after it is created by the program. "
                    putStrLn ""
                    let xs = unwords . drop 2 $ args
                        ws = snd . genControl . concat $ arg
                        ys = take (nSymbols (if null arg then [] else fst . genControl . head $ arg)) xs
                    withBinaryFile (nameSF ++ ".raw") AppendMode (appendS16LEFileList (convertToProperUkrainian ys, show7s xs))
                    putStrLn "The .raw file was created by the program. It will be processed further. "
                    let ts = fromJust (showE "sox") in do
                      _ <- readProcessWithExitCode ts (case fst ws of
                             "" -> ["-r22050","-c1","-L","-esigned-integer","-b16", nameSF ++ ".raw", nameSF ++ snd ws]
                             _  -> ["-r22050","-c1","-L","-esigned-integer","-b16", nameSF ++ ".raw", fst ws, nameSF ++ snd ws]) ""
                      removeFile $ nameSF ++ ".raw"
                      if take 5 os == "mingw"
                        then do
                          _ <- readProcessWithExitCode (fromJust . showE $ "sox") [nameSF ++ snd ws, "-t", "waveaudio", "-d"] ""
                          return ()
                        else if isJust . showE $ "play"
                               then do
                                 _ <- readProcessWithExitCode (fromJust . showE $ "play") [nameSF ++ snd ws] ""
                                 return ()
                               else catchEnd ExecutableNotProperlyInstalled
                else do
                  [xs, wws] <- mapM defineClean ([0,1]::[Int])
                  if wws == "y" then do
                    let ys = take (nSymbols (if null arg then [] else fst . genControl . head $ arg)) xs
                    withBinaryFile (nameSF ++ ".raw") AppendMode (appendS16LEFile (convertToProperUkrainian ys))
                    putStrLn "The .raw file was created by the program. It will be processed further. "
                    let ts = fromJust (showE "sox") in do
                      let ws = snd . genControl . concat $ arg
                      _ <- readProcessWithExitCode ts ["-r22050","-c1","-L","-esigned-integer","-b16", nameSF ++ ".raw",
                             fst ws, nameSF ++ snd ws] ""
                      removeFile $ nameSF ++ ".raw"
                      if take 5 os == "mingw"
                        then do
                          _ <- readProcessWithExitCode (fromJust . showE $ "sox") [nameSF ++ snd ws, "-t", "waveaudio", "-d"] ""
                          cleanCreatedSoundFs
                        else if isJust . showE $ "play"
                               then do
                                 _ <- readProcessWithExitCode (fromJust . showE $ "play") [nameSF ++ snd ws] ""
                                 cleanCreatedSoundFs
                               else catchEnd ExecutableNotProperlyInstalled
                  else do
                    let ws = snd . genControl . concat $ arg
                        ys = take (nSymbols (if null arg then [] else fst . genControl . head $ arg)) xs
                    withBinaryFile (nameSF ++ ".raw") AppendMode (appendS16LEFile (convertToProperUkrainian ys))
                    putStrLn "The .raw file was created by the program. It will be processed further. "
                    let ts = fromJust (showE "sox") in do
                      _ <- readProcessWithExitCode ts (case fst ws of
                             "" -> ["-r22050","-c1","-L","-esigned-integer","-b16", nameSF ++ ".raw", nameSF ++ snd ws]
                             _  -> ["-r22050","-c1","-L","-esigned-integer","-b16", nameSF ++ ".raw", fst ws, nameSF ++ snd ws]) ""
                      removeFile $ nameSF ++ ".raw"
                      if take 5 os == "mingw"
                        then do
                          _ <- readProcessWithExitCode (fromJust . showE $ "sox") [nameSF ++ snd ws, "-t", "waveaudio", "-d"] ""
                          return ()
                        else if isJust . showE $ "play"
                               then readProcessWithExitCode (fromJust . showE $ "play") [nameSF ++ snd ws] "" >> return ()
                               else catchEnd ExecutableNotProperlyInstalled)

-- | Used internally in the 'main7h' function.
giveInfo :: IO ()
giveInfo = do
  dir <- getCurrentDirectory
  putStrLn "You are now in the directory: "
  putStrLn $ show dir
  putStrLn ""
  putStr "You could specify a name of the resulting file and then the control parameters for the output speech file as "
  putStrLn "the second command line argument to the running program mmsyn7h! "
  putStrLn "See https://hackage.haskell.org/package/mmsyn6ukr-0.8.0.0/docs/UkrainianLControl.html#v:genControl for more information."
  putStr "You could specify e. g. \"o9-1\" or \"o5-1\" (and the most compressed audio in the .ogg format will be produced) or other option. "
  putStrLn "If you have not specified the name and the parameters and now would like to, please, terminate the running program and execute it again with the proper command line arguments. "
  putStrLn ""
  putStr "If you specified further command line arguments as a Ukrainian text, that contains only those sounds, which sound representations are in the current directory "
  putStr "(you can create them by e. g. mmsyn7ukr and mmsyn7l programs in the same name packages), then the program will use only these sounds representations "
  putStrLn "additionally to the default ones \"-.wav\", \"0.wav\" and \"1.wav\". See further: https://hackage.haskell.org/package/mmsyn7s"
  putStrLn ""

-- | Used internally in the 'main7h' function for specifying whether clean the current directory from the sound files.
defineClean :: Int -> IO String
defineClean x
  | x == 0 = onException (do
     putStrLn ""
     putStr "The resulting file will be played just after it is created by the program. "
     putStrLn ""
     putStrLn "Now enter the Ukrainian text."
     putStrLn ""
     xs <- getLine
     return xs ) (do { putStrLn "The process was not successful may be because of the not valid data specified. Please, specify valid data. "
                     ; defineClean 0 })
  | otherwise = onException (do
     putStr "Would you like to remove all the sound files created in the directory after playback? If yes, then enter here \"y\". Otherwise, "
     putStrLn "the files will not be removed by the program. "
     ys <- getLine
     let zs = take 1 ys in return zs )
       (do { putStr "The process was not successful may be because of the not valid data specified. "
           ; putStrLn "Please, specify valid data. "
           ; defineClean 1 })

-- | The function that actually produces a .raw file. The mapping table is given in the @Map.txt@ file, but the sound duration differs.
appendS16LEFile ::  V.Vector String -> Handle -> IO ()
appendS16LEFile xs hdl | not (V.null xs) =
  do
    dataList <- V.mapM takeData (V.fromList ["-.wav", "0.wav", "1.wav", "A.wav", "B.wav", "C.wav", "D.wav", "E.wav", "F.wav", "G.wav", "H.wav",
        "I.wav", "J.wav", "K.wav", "L.wav", "M.wav", "N.wav", "O.wav", "P.wav", "Q.wav", "R.wav",
          "S.wav", "T.wav", "U.wav", "V.wav", "W.wav", "X.wav", "Y.wav", "Z.wav", "a.wav", "b.wav", "c.wav",
            "d.wav", "e.wav", "f.wav"])
    V.mapM_ (\u ->
      if V.all (\z -> BS.length z > 0) dataList
        then let rs =  tail . dropWhile (/= ' ') . takeWhile (/= '}') . show $ hdl in do
          hClose hdl
          closedHdl <- hIsClosed hdl
          if closedHdl
            then BS.appendFile rs $ dataList V.! getBFst' (0, V.fromList [("-", 0), ("0", 1), ("1", 2), ("а", 3), ("б", 4),
                     ("в", 5), ("г", 6), ("д", 7), ("дж", 8), ("дз", 9), ("е", 10), ("ж", 11), ("з", 12), ("и", 13),
                        ("й", 14), ("к", 15), ("л", 16), ("м", 17), ("н", 18), ("о", 19), ("п", 20), ("р", 21),
                          ("с", 22), ("сь", 23), ("т", 24), ("у", 25), ("ф", 26), ("х", 27), ("ц", 28), ("ць", 29), ("ч", 30),
                            ("ш", 31), ("ь", 32), ("і", 33), ("ґ", 34)]) u
            else catchEnd (DataFileNotClosed (show hdl))
        else catchEnd (DataSoundFileNotRead "")) xs
    hClose hdl
                       | otherwise = return ()

-- | The function that actually produces a .raw file. The mapping table is that one given in the @Map.txt@ file but not all sounds and files are present.
-- The @[String]@ parameter is a sorted list of Ukrainian sounds to be used (for example, it can be obtained with @mmsyn7s@ executable from the same name package).
appendS16LEFileList :: (V.Vector String, [String]) -> Handle -> IO ()
appendS16LEFileList (xs, yss) hdl | not (V.null xs) && not (null yss) =
  do
    let intrm = map (getBFst' ("0.wav", V.fromList . zip ["а","б","в","г","д","дж","дз","е","ж","з","и","й","к","л","м","н","о","п","р","с",
                       "сь","т","у","ф","х","ц","ць","ч","ш","ь","і","ґ"] $ ["A.wav", "B.wav", "C.wav", "D.wav", "E.wav", "F.wav", "G.wav", "H.wav",
                         "I.wav", "J.wav", "K.wav", "L.wav", "M.wav", "N.wav", "O.wav", "P.wav", "Q.wav", "R.wav",
                           "S.wav", "T.wav", "U.wav", "V.wav", "W.wav", "X.wav", "Y.wav", "Z.wav", "a.wav", "b.wav", "c.wav",
                             "d.wav", "e.wav", "f.wav"])) yss
    dataList <- (V.mapM takeData . V.fromList) (["-.wav", "0.wav", "1.wav"] ++ intrm)
    V.mapM_ (\u ->
      if V.all (\z -> BS.length z > 0) dataList
        then let rs =  tail . dropWhile (/= ' ') . takeWhile (/= '}') . show $ hdl in do
          hClose hdl
          closedHdl <- hIsClosed hdl
          if closedHdl
            then BS.appendFile rs $ dataList V.! getBFst' (0, V.fromList $ [("-", 0), ("0", 1), ("1", 2)] ++ zip yss [3..]) u
            else catchEnd (DataFileNotClosed (show hdl))
        else catchEnd (DataSoundFileNotRead "")) xs
    hClose hdl
                                  | otherwise = return ()

-- | Function that removes all the sounds with ".raw", ".wav", ".ogg", ".flac" extensions in the current directory. It is used for
-- the security reasons.
cleanCreatedSoundFs :: IO ()
cleanCreatedSoundFs = do
  dirCs <- listDirectory "."
  let remFs = concatMap (\ys -> filter (\zs -> ys `isSuffixOf` zs) dirCs) [".raw", ".wav", ".ogg", ".flac"] in mapM_ removeFile remFs
