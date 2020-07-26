{-# LANGUAGE TemplateHaskell #-}
module Paracletus.TH where
-- we use TemplateHaskell to compile shaders
import Prelude()
import UPrelude
import Control.Arrow (first, second)
import Control.Monad (unless, when)
import Data.Char
import Data.List
import Data.Maybe (fromMaybe)
import Foreign.Marshal.Array
import GHC.Ptr (Ptr (..))
import Language.Haskell.TH
import System.Directory
import System.Exit
import System.FilePath
import System.IO
import System.Process

compileGLSL ∷ FilePath → ExpQ
compileGLSL fpath = do
  (spirvFile, (ec, stdo, stde)) ← runIO $ do
    validatorExe ← fromMaybe ( error "cannot find glslangValidator" ) ⊚ findExecutable "glslangValidator"
    tmpDir ← getTemporaryDirectory
    curDir ← getCurrentDirectory
    createDirectoryIfMissing True tmpDir
    let spirvCodeFile = tmpDir ⊘ "haskell-spirv.tmp"
        shaderFile = curDir ⊘ fpath
        shaderDir = takeDirectory shaderFile
        shaderFName = takeFileName shaderFile
    doesFileExist shaderFile ⌦ flip unless (error $ shaderFile ⧺ " does not exist")
    doesFileExist spirvCodeFile ⌦ flip when (removeFile spirvCodeFile)
    (,) spirvCodeFile ⊚ readCreateProcessWithExitCode (shell $ validatorExe ⧺ " -V -o " ⧺ spirvCodeFile ⧺ " " ⧺ shaderFName) { cwd = Just shaderDir } ""
  runQ ∘ reportGlslMsgs $ unlines [stdo, stde]
  case ec of
    ExitSuccess → pure ()
    ExitFailure i → error $ "glslangValidator exited with code " ⧺ show i
  contents ← runIO . withBinaryFile spirvFile ReadMode $ \h → do
    fsize ← hFileSize h
    let contentSize = fromIntegral $ case rem fsize 4 of
          0 → fsize
          k → fsize + 4 - k
    allocaArray contentSize $ \ptr → do
      hasRead ← hGetBuf h ptr contentSize
      (⧺ replicate (contentSize - hasRead) 0) ⊚ peekArray hasRead ptr
  return $ TupE [LitE . IntegerL . fromIntegral $ length contents, AppE (ConE 'Ptr) (LitE $ StringPrimL contents)]

reportGlslMsgs ∷ String → Q ()
reportGlslMsgs s = case parseValidatorMsgs s of
  (warns, errs) → do
    mapM_ reportWarning warns
    mapM_ reportError errs

parseValidatorMsgs ∷ String → ([String], [String])
parseValidatorMsgs = go ∘ map strip ∘ lines
  where strip = dropWhileEnd isSpace ∘ dropWhile isSpace
        go [] = ([],[])
        go (x:xs) | "WARNING:" `isPrefixOf` x = first  (strip (drop 8 x):) $ go xs
                  | "ERROR:"   `isPrefixOf` x = second (strip (drop 6 x):) $ go xs
                  | otherwise = go xs
