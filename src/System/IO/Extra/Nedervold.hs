{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module System.IO.Extra.Nedervold
  ( writeFileWithConfig
  , Config(..)
  ) where

import Control.Exception
import Control.Monad (unless)
import System.Directory (doesFileExist)
import System.IO (readFile')

data Config =
  Config
    { fallibleFilter :: Bool
    -- ^ If the filter throws an 'IOError', pass the unfiltered contents instead.
    , filterContents :: Maybe (String -> IO String)
    -- ^ Filter the file contents.  'System.Process.readProcess' and
    -- 'System.Process.readCreateProcess' might be useful.
    , iffChanged :: Bool
    -- ^ Write if and only if the file contents change.
    }

filterFileContents :: Config -> String -> IO String
filterFileContents Config {..} str = do
  case filterContents of
    Nothing -> pure str
    Just f ->
      if fallibleFilter
        then f str `catch` catchIO
        else f str
  where
    catchIO :: IOError -> IO String
    catchIO _ = pure str

writeFileIffChanged :: FilePath -> String -> IO ()
writeFileIffChanged fp str = do
  exists <- doesFileExist fp
  if exists
      -- TODO I don't necessarily /need/ to read the whole file, just
      -- enough to ensure inequality.  But I also need to make sure
      -- the handle is closed before moving on.  So 'readFile'' could
      -- be overkill.  Do something better, or document why this is
      -- the right choice.
    then do
      current <- readFile' fp
      unless (current == str) $ writeFile fp str
    else writeFile fp str

writeFileWithConfig :: Config -> FilePath -> String -> IO ()
writeFileWithConfig config fp str = do
  filtered <- filterFileContents config str
  if iffChanged config
    then writeFileIffChanged fp filtered
    else writeFile fp filtered
