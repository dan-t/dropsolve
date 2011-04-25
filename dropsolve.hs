import System (getArgs)
import System.Directory
import System.Posix.Files
import System.IO
import System.FilePath
import System.FilePath.Posix
import System.Process
import Data.List (isInfixOf, isSuffixOf, concat)
import Data.Char (intToDigit, digitToInt, isDigit, toUpper)
import Text.Regex.Posix ((=~))
import Control.Monad (when, mapM_, filterM)

main = do
   hSetBuffering stdout NoBuffering
   args <- getArgs
   case args of
	[]            -> printHelp
        ("-h":[])     -> printHelp
	("--help":[]) -> printHelp
	(dropDir:[])  -> resolve dropDir
	otherwise     -> error $ "Invalid Arguments\n" ++ usage

printHelp = putStrLn usage
usage = "Usage: dropsolve DROPBOXDIR"

resolve file = do
   exist <- fileExist file
   when exist $ do
      fileStatus <- getFileStatus file
      if isDirectory fileStatus
	 then do
	    entries <- getDirContents file
	    mapM_ (\e -> resolve $ file </> e) entries
	 else do
	    when (hasConflict file) $ handleConflict file

hasConflict file = "conflicted copy" `isInfixOf` file

handleConflict file = do
   exist <- fileExist file
   when exist $ do
      let confInfo = conflictInfo file
      confFiles <- findConflicting confInfo
      resolveConflict confInfo confFiles 

   where
      findConflicting confInfo = do
	 let d  = dir confInfo
	     fn = fileName confInfo
	 entries <- getDirContents d
	 let confs = filter (isConfFile fn) entries
	 mapM (\e -> return $ d </> e) confs
	 where
	    isConfFile file = \e -> file `isInfixOf` e && hasConflict e

      resolveConflict confInfo confFiles
	 | length confFiles == 0 = return ()
	 | otherwise = do
	    let origFile = dir confInfo ++ fileName confInfo
	    putStrLn $ "Conflicting file: " ++ origFile
            putConfFiles confFiles 1
	    askUser confInfo confFiles

      putConfFiles (c:cs) num = do
	 let confInfo = conflictInfo c
	     h        = host confInfo
	     d        = date confInfo
	     digit    = intToDigit num
	 putStrLn $ "   (" ++ [digit] ++ ") " ++ h ++ " from " ++ d
	 putConfFiles cs (num + 1)
      putConfFiles [] _ = return ()

      askUser confInfo confFiles = do
	 putStr "Take (NUM) | (D)elete confs | (S)how diff | (I)gnore: " 
	 char <- getChar
	 -- catch newline
	 getChar
	 let numConfs = length confFiles
	 case toUpper char of
	      c | c == 'S' && numConfs >= 2             -> showDiff (head confFiles) (confFiles !! 1) >> askUser confInfo confFiles
	        | c == 'D'                              -> mapM_ (\c -> deleteFile c) confFiles  
	        | c == 'I'                              -> return ()
		| isDigit c && digitToInt c <= numConfs -> takeFile (digitToInt c) confInfo confFiles
		| otherwise                             -> askUser confInfo confFiles
	 where
	    deleteFile file = do
	       appDir <- getAppUserDataDirectory "dropsolve"
	       let trash           = appDir </> "trash"
		   (dir, fileName) = splitFileName file
	       copyFile file (trash </> fileName) 
	       removeFile file

	    takeFile num confInfo confFiles = do
	       let idx  = num - 1
                   file = confFiles !! idx
		   d    = dir confInfo
		   fn   = fileName confInfo
	       copyFile file (d </> fn)
	       mapM_ (\c -> deleteFile c) confFiles

	    showDiff file1 file2 = do
	       runCommand $ "gvimdiff -f " ++ quote file1 ++ " " ++ quote file2 ++ ">\& /dev/null"
	       return ()

	    quote string = "\"" ++ string ++ "\""


-- conflicting file info
data ConflictInfo = ConflictInfo {
   filePath :: String,
   dir      :: String,
   fileName :: String,
   host     :: String,
   date     :: String }

conflictInfo :: FilePath -> ConflictInfo
conflictInfo filePath =
   let (_:dir:fileName:host:date:[]) = concat (filePath =~ regex :: [[String]])
       in ConflictInfo filePath dir fileName host date
   where
      regex = "(.*)" </> "(.*) \\((.*) conflicted copy (.*)\\).*"


getDirContents dir = do
   entries <- getDirectoryContents dir
   filterM notDots entries
   where
      notDots entry = return . not $ "." `isSuffixOf` entry || ".." `isSuffixOf` entry
