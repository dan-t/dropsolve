import System (getArgs)
import System.Directory
import System.Posix.Files
import System.IO
import System.FilePath
import System.FilePath.Posix
import System.Process
import System.Exit
import Data.Time.Clock
import Data.Time.Calendar
import Data.List (isInfixOf, isSuffixOf, concat)
import Data.Char (intToDigit, digitToInt, isDigit, toUpper)
import Text.Regex.Posix ((=~))
import Control.Monad (when, mapM_, filterM)

main = do
   hSetBuffering stdout NoBuffering
   args <- getArgs
   case args of
	[]            -> printUsage >> printHelp
        ("-h":[])     -> printUsage >> printHelp
	("--help":[]) -> printUsage >> printHelp
	(dropDir:[])  -> resolve dropDir
	otherwise     -> error $ "Invalid Arguments\n" ++ usage

printUsage = putStrLn usage
usage = "\nUsage: dropsolve DROPBOXDIR"

printHelp = do
   trashDir <- trashDirectory
   putStrLn $ ""
   putStrLn $ "Runtime options:"
   putStrLn $ "   Take File (NUM) => By pressing a digit, the conflicting file with the"
   putStrLn $ "                      digit NUM is used as the new version. A copy of the"
   putStrLn $ "                      current file and the other conflicting files is put"
   putStrLn $ "                      into the trash directory."
   putStrLn $ ""
   putStrLn $ "   Move to (T)rash => By pressing 'T' or 't', all conflicting files are"
   putStrLn $ "                      moved into the trash directory (" ++ trashDir ++ ")."
   putStrLn $ ""
   putStrLn $ "   Show (D)iff     => By pressing 'D' or 'd', the difference between the first"
   putStrLn $ "                      and the second conflicting file is shown."
   putStrLn $ ""
   putStrLn $ "   (S)kip          => By pressing 'S' or 's', the current conflict is skipped"
   putStrLn $ "                      and the next one is shown."
   putStrLn $ ""
   putStrLn $ "   (Q)uit          => By pressing 'Q' or 'q', the application is quit."
   putStrLn $ ""
   putStrLn $ "   (H)elp          => By pressing 'H' or 'h', this help is printed."
   putStrLn $ ""

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
	    let origFile = dir confInfo </> fileName confInfo
	    putStrLn $ "\nConflicting file: " ++ origFile
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
	 putStr "\nTake File (NUM) | Move to (T)rash | Show (D)iff | (S)kip | (Q)uit | (H)elp : " 
	 char <- getChar
	 -- catch newline
	 getChar
	 let numConfs = length confFiles
	 case toUpper char of
	      c | c == 'D' && numConfs >= 2             -> showDiff (head confFiles) (confFiles !! 1) >> askUser confInfo confFiles
	        | c == 'T'                              -> mapM_ (\c -> moveToTrash c) confFiles  
	        | c == 'S'                              -> return ()
		| c == 'Q'                              -> exitSuccess
		| c == 'H'                              -> printHelp
		| c == '?'                              -> printHelp
		| isDigit c && digitToInt c <= numConfs -> takeFile (digitToInt c) confInfo confFiles
		| otherwise                             -> askUser confInfo confFiles
	 where
	    moveToTrash file = do
	       appDir      <- appDirectory
	       appDirExist <- doesDirectoryExist appDir
	       when (not appDirExist) $ createDirectory appDir 
	       trashDir    <- trashDirectory
	       trashExist  <- doesDirectoryExist trashDir
	       when (not trashExist) $ createDirectory trashDir
               let (dir, fileName) = splitFileName file
	       copyFile file (trashDir </> fileName) 
	       removeFile file

	    takeFile num confInfo confFiles = do
               (year, month, day) <- getCurrentDate
	       let idx        = num - 1
                   file       = confFiles !! idx
		   origFile   = dir confInfo </> fileName confInfo
		   origBackup = origFile ++ "_backup_" ++ show year ++ "-" ++ show month ++ "-" ++ show day
	       copyFile origFile origBackup
	       moveToTrash origBackup
	       copyFile file origFile
	       mapM_ (\c -> moveToTrash c) confFiles

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


appDirectory   = getAppUserDataDirectory "dropsolve"
trashDirectory = appDirectory >>= \d -> return $ d </> "trash" 

getCurrentDate :: IO (Integer,Int,Int) -- :: (year,month,day)
getCurrentDate = getCurrentTime >>= return . toGregorian . utctDay