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

module MailParser where
import Text.ParserCombinators.Parsec
import Data.String.Utils
import Types(sanitize)

eol = (string "\r\n") <|> string "\n"

line = do l <- many (noneOf "\r\n")
          eol
          return l

line1 = do l <- many1 (noneOf "\r\n")
           eol
           return l

header =
    do c1 <- (refHdr <|> (line1 >> return ""))
       return c1

refHdr = 
    do try (string "References: ")
       c <- line
       c2 <- restOfHdr
       return (c ++ c2)
    where restOfHdr =
              (do oneOf " \t"
                  r <- line
                  n <- restOfHdr
                  return (r ++ n)) <|> return ""
                 
headers = many1 header

maybeFrom = try (do string "From "
                    many1 (noneOf "\r\n")
                    eol
                    return ()
                ) <|> (return ())

body = do b <- many anyChar
          eof
          return b

message =
    do maybeFrom
       h <- headers
       eol
       b <- body
       return (strip (concat h), strip . sanitize $ b)
