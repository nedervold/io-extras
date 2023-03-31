-- | Extra functionality for writing to files.  Allows filtering the
-- contents, possibly through an external program, and writing contents
-- only if changed.
--
-- Useful for code generation: generated code can be filtered through
-- an external formatter and timestamps change only when file contents
-- change.
module System.IO.Extra.Nedervold
  ( writeFileWithConfig
  , Config(..)
  ) where

import Control.Exception
import Control.Monad (unless)
import System.Directory (doesFileExist)
import System.IO (readFile')

-- | Configuration of the writing.
data Config =
  Config
    { filterContents :: Maybe (String -> IO String)
    -- ^ Possibly filter the file contents. To filter through an
    -- external program, 'System.Process.readProcess' and
    -- 'System.Process.readCreateProcess' might be useful.
    , fallibleFilter :: Bool
    -- ^ If 'True' and the filter fails with an 'IOError', catch the
    -- exception and pass the unfiltered contents.
    , iffChanged :: Bool
    -- ^ Write the contents if and only if the file contents will
    -- change.
    }

-- | Filter the file contents using the configuration.
filterFileContents :: Config -> String -> IO String
filterFileContents config str = do
  case filterContents config of
    Nothing -> pure str
    Just f ->
      if fallibleFilter config
        then f str `catch` catchIO
        else f str
  where
    catchIO :: IOError -> IO String
    catchIO _ = pure str

-- | Write the file [if and only
-- if](https://en.wikipedia.org/wiki/If_and_only_if) the contents will
-- change.
writeFileIffChanged :: FilePath -> String -> IO ()
writeFileIffChanged fp str = do
  exists <- doesFileExist fp
  if exists
      -- I don't need to read the /whole/ file, just enough to ensure
      -- inequality.  But I also need to make sure the read handle is
      -- closed before writing to it or strange things can happen.  So
      -- 'readFile'' may be overkill.  TODO Do something better, or
      -- document why 'readFile'' is the right choice.
    then do
      current <- readFile' fp
      unless (current == str) $ writeFile fp str
    else writeFile fp str

-- | Write the string to the file using the configuration.
writeFileWithConfig :: Config -> FilePath -> String -> IO ()
writeFileWithConfig config fp str = do
  filtered <- filterFileContents config str
  if iffChanged config
    then writeFileIffChanged fp filtered
    else writeFile fp filtered
