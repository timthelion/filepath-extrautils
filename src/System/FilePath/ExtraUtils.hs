{-# LANGUAGE PackageImports #-}
module System.FilePath.ExtraUtils
 (getParentNamedMaybe
 ,walkUpPathTill
 ,isPathTo
 ,clipPaths
 ,clipPath
 ,getRealDirectoryContents
 ,getDirectoryContentsRecursive
 ,getFilesInDirectoryRecursive
 ,getSubDirectoryContentsRecursive
 ,getSubDirectories) where

import "base" Control.Monad
 (filterM)

import "directory" System.Directory
 (doesFileExist
 ,doesDirectoryExist
 ,getDirectoryContents)

import "unix-compat" System.PosixCompat.Files
 (getSymbolicLinkStatus
 ,isSymbolicLink)

import "filepath" System.FilePath
 ((</>)
 ,joinPath
 ,splitDirectories
 ,splitPath)

getParentNamedMaybe :: FilePath -> FilePath -> Maybe FilePath
getParentNamedMaybe = walkUpPathTill

-- | walkUpPathTill "foo" "/home/tim/foo/bar/log" => Just "/home/tim/foo"
--   walkUpPathTill "foo" "/home/tim/baz/bog" => Nothing
walkUpPathTill :: FilePath -> FilePath -> Maybe FilePath
walkUpPathTill goal path
 = walkUpPathTill' goal
 $ reverse
 $ splitDirectories path

walkUpPathTill' goal (i:is) =
 case i == goal of
  True  -> Just $ joinPath $ reverse (i:is)
  False -> walkUpPathTill' goal is
walkUpPathTill' _ [] = Nothing

-- | isPathTo "/home/tim/bar/baz" "baz" => True
--   isPathTo "/home/tim/foo/bog" "baz" => False
--   isPathTo "/home/tim/baz/bog" "baz" => False
isPathTo :: FilePath -> FilePath -> Bool
isPathTo path destination = destination == (last $ splitPath path)

-- | clipPaths ["/home/tim/baz","/home/tim/bar"] => ["/home/tim","/home/tim"]
clipPaths :: [FilePath] -> [FilePath]
clipPaths = map clipPath

-- | clipPath "/home/tim/baz" -> "/home/tim"
clipPath :: FilePath -> FilePath
clipPath path
 = joinPath
 $ init
 $ splitPath path

-- | Returns relative paths to all files and directories which are bellow the current directory.  i.e. the contents of this directory, all of it's subdirectories and their subdirectories and so on. 
-- NOTE: does not return the contents of subdirectories which are symlinks.
getDirectoryContentsRecursive :: FilePath -> IO [FilePath]
getDirectoryContentsRecursive dir = do
 contents       <- getDirectoryContents dir
 subDirs        <- getSubDirectories    dir
 contentsOfSubDirs <-
  mapM
     getDirectoryContentsRecursive
     subDirs
 return
  $ concat $ contents : contentsOfSubDirs

getFilesInDirectoryRecursive :: FilePath -> IO [FilePath]
getFilesInDirectoryRecursive dir = do
 fullContents <- getDirectoryContentsRecursive dir
 filterM doesFileExist fullContents

-- | List all the files and directories in the subdirectories of a directory.
getSubDirectoryContentsRecursive :: FilePath -> IO [FilePath]
getSubDirectoryContentsRecursive topDir = do
 subDirs <- getSubDirectories topDir
 contents <- mapM getDirectoryContentsRecursive $ map (\dir -> topDir </> dir) subDirs
 return $ concat contents

-- Shamelessly stolen (AND FIXED!) from the Extra package(due to dependencies)
-- | Return the list of subdirectories, omitting . and ..
getSubDirectories :: FilePath -> IO [String]
getSubDirectories path =
 getRealDirectoryContents path >>=
 filterM isRealDirectory
 where
   isRealDirectory name = do
    status <- getSymbolicLinkStatus (path </> name)
    isDirectory <- doesDirectoryExist (path </> name)
    return $ (not $ isSymbolicLink status) && isDirectory

-- | return the real directory contents, without the "." and the ".."
getRealDirectoryContents
 :: FilePath
 -> IO [FilePath]
getRealDirectoryContents directory =
 getDirectoryContents directory >>=
 return . filter (not . (flip elem) [".", ".."])