module Main where

import Distribution.Simple
import Distribution.Simple.Utils
import Distribution.Simple.Setup
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Install
import Distribution.Simple.UUAGC (uuagcLibUserHook)
import System.Directory
import UU.UUAGC (uuagc)

main :: IO()
main = do let uuagcHook   = uuagcLibUserHook uuagc
              browserHook = uuagcHook {instHook = myInstallHook}
          defaultMainWithHooks browserHook

myInstallHook pkg_descr localbuildinfo _ flags 
    = do let copyFlags = defaultCopyFlags {
                      copyDistPref   = installDistPref flags,
                      copyDest       = toFlag NoCopyDest,
                      copyVerbosity  = installVerbosity flags
                  }
         install pkg_descr localbuildinfo copyFlags
         createAndCopyConfigDir flags

createAndCopyConfigDir flags
    = do let verbosity = fromFlag (installVerbosity flags)
         pathConfig <- getAppUserDataDirectory "3SFWebBrowser"
         notice verbosity ("Creating config directory at " ++ pathConfig)
         createDirectoryIfMissingVerbose verbosity True pathConfig 
         installDirectoryContents verbosity "config/" pathConfig
