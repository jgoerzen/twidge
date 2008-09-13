{-
Copyright (C) 2006-2008 John Goerzen <jgoerzen@complete.org>

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
-}

{- |
   Module     : Utils
   Copyright  : Copyright (C) 2006-2008 John Goerzen
   License    : GNU GPL, version 2 or above

   Maintainer : John Goerzen <jgoerzen@complete.org>
   Stability  : provisional
   Portability: portable

Written by John Goerzen, jgoerzen\@complete.org

-}

module Utils where
import System.Console.GetOpt
import Types
import System.Exit
import System.IO
import Data.ConfigFile
import Text.Regex.Posix
import Data.Either.Utils(forceEither)
import Network.URI
import Data.Maybe.Utils
import Text.Printf

simpleCmd :: String             -- ^ Command name
          -> String             -- ^ Command description
          -> String             -- ^ Command help text
          -> [OptDescr (String, String)] -- ^ Option descriptions
          -> (Maybe String -> ConfigParser -> ([(String, String)], [String]) -> IO ()) -- ^ Function to call 
          -> (String, Command)
simpleCmd name descrip helptext optionsinp func =
    (name, Command {cmdname = name, cmddescrip = descrip,
                    execcmd = worker})
    where options =
              optionsinp ++ [Option "" ["help"] (NoArg ("help", "")) "Display this help"]
          worker argv cpath gi =
              case getOpt RequireOrder options argv of
                (o, n, []) -> 
                    if (lookup "help" o == Just "") 
                       then usageerror []
                       else func cpath gi (o, n)
                (_, _, errors) -> usageerror (concat errors)
          usageerror errormsg =
              do putStrLn $ "Error processing arguments for command " ++ 
                          name ++ ":"
                 putStrLn errormsg
                 putStrLn (usageInfo header options)
                 putStrLn helptext
                 exitFailure
          header = "Available command-options for " ++ name ++ " are:\n"
                                                               
{-
lock func =
    do appdir <- getAppDir
       lockh <- openFile (appdir ++ "/.lock") WriteMode
       lockfd <- handleToFd lockh
       catch (placelock lockfd) errorhandler
       r <- finally func (releaselock lockfd)
       return r

    where placelock lockfd = setLock lockfd (WriteLock, AbsoluteSeek, 0, 0)
          releaselock lockfd = do
               setLock lockfd (Unlock, AbsoluteSeek, 0, 0)
               closeFd lockfd
          errorhandler _ =
              do putStrLn "Aborting because another twidge is already running"
                 exitFailure
-}

ex_tempfail = 75
ex_permfail = 69

permFail :: String -> IO a
permFail msg =
    do hPutStrLn stderr msg
       exitWith (ExitFailure ex_permfail)

serverHost cp = host
    where urlbase = forceEither $ get cp "DEFAULT" "urlbase"
          uri = forceMaybeMsg "genMsgId parseURI" $ parseURI urlbase
          host = uriRegName . forceMaybeMsg "genMsgId uriauth" . 
                 uriAuthority $ uri

genMsgId :: String -> Message -> ConfigParser -> String
genMsgId section m cp =
    printf "<%s.%s.%s@%s.%s.twidge>" (sId m) (sSender m) (sRecipient m)
               section (serverHost cp) 
-- FIXME: escape periods in serverHost

{- | Parses a message id, returning (Message, host, section).
The sText and sDate fiels will be empty. -}
parseMsgId :: String -> Maybe (Message, String, String)
parseMsgId msgid =
    case msgid =~ repat of
      [[_, id, sender, recipient, section, host]] -> 
          Just (Message {sId = id, sSender = sender, sRecipient = recipient, 
                         sText = "", sDate = ""},
                host, section)
      _ -> Nothing
    where repat = "^<([^.@]+)\\.([^.@]+)\\.([^@.]*)@([^.]+)\\.(.+)\\.twidge>"

----------------------------------------------------------------------
-- Start of code from Cabal 1.4.0.2

-- | Wraps text to the default line width. Existing newlines are preserved.
wrapText :: String -> String
wrapText = unlines
         . concatMap (map unwords
                    . wrapLine 79
                    . words)
         . lines

-- | Wraps a list of words to a list of lines of words of a particular width.
wrapLine :: Int -> [String] -> [[String]]
wrapLine width = wrap 0 []
  where wrap :: Int -> [String] -> [String] -> [[String]]
        wrap 0   []   (w:ws)
          | length w + 1 > width
          = wrap (length w) [w] ws
        wrap col line (w:ws)
          | col + length w + 1 > width
          = reverse line : wrap 0 [] (w:ws)
        wrap col line (w:ws)
          = let col' = col + length w + 1
             in wrap col' (w:line) ws
        wrap _ []   [] = []
        wrap _ line [] = [reverse line]

-- End of code from Cabal 1.4.0.2
----------------------------------------------------------------------
