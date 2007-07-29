{- hpodder component
Copyright (C) 2006-2007 John Goerzen <jgoerzen@complete.org>

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
   Module     : DB
   Copyright  : Copyright (C) 2006-2007 John Goerzen
   License    : GNU GPL, version 2 or above

   Maintainer : John Goerzen <jgoerzen@complete.org>
   Stability  : provisional
   Portability: portable

Written by John Goerzen, jgoerzen\@complete.org

-}
module DB where
import Config
import Types

import Database.HDBC
import Database.HDBC.Sqlite3
import System.Log.Logger
import Control.Monad
import Control.Exception
import Utils
import Data.List.Utils
dbdebug = debugM "DB"

connect :: IO Connection
connect = handleSqlError $
    do fp <- getDBName
       dbh <- connectSqlite3 fp
       setBusyTimeout dbh 5000
       prepDB dbh
       dbdebug $ "DB preparation complete"
       return dbh

prepDB dbh =
    do tables <- getTables dbh
       evaluate (length tables)
       schemaver <- prepSchema dbh tables
       upgradeSchema dbh schemaver tables

prepSchema :: Connection -> [String] -> IO Int
prepSchema dbh tables =
    if "schemaver" `elem` tables
       then do r <- quickQuery dbh "SELECT version FROM schemaver" []
               case r of
                 [[x]] -> return (fromSql x)
                 x -> fail $ "Unexpected result in prepSchema: " ++ show x
       else do dbdebug "Initializing schemaver to 0"
               run dbh "CREATE TABLE schemaver (version INTEGER)" []
               run dbh "INSERT INTO schemaver VALUES (0)" []
               commit dbh
               return 0

upgradeSchema dbh 4 _ = return ()

upgradeSchema dbh 3 tables =
    do dbdebug "Upgrading schema 3 -> 4"
       dbdebug "Adding lastattempt column"
       run dbh "ALTER TABLE podcasts ADD lastattempt INTEGER" []
       dbdebug "Adding failedattempts column"
       run dbh "ALTER TABLE podcasts ADD failedattempts INTEGER NOT NULL DEFAULT 0" []

       dbdebug "Adding epfirstattempt column"
       run dbh "ALTER TABLE episodes ADD epfirstattempt INTEGER" []
       dbdebug "Adding eplastattempt column"
       run dbh "ALTER TABLE episodes ADD eplastattempt INTEGER" []
       dbdebug "Adding epfailedattempts column"
       run dbh "ALTER TABLE episodes ADD epfailedattempts INTEGER NOT NULL DEFAULT 0" []

       setSchemaVer dbh 4
       commit dbh

upgradeSchema dbh 2 tables =
    do dbdebug "Upgrading schema 2 -> 3"
       dbdebug "Adding eplength column"
       run dbh "ALTER TABLE episodes ADD eplength INTEGER NOT NULL DEFAULT 0" []
       setSchemaVer dbh 3
       commit dbh
       
       -- Empty the enclosure storage since our naming changed when this
       -- version arrived
       edir <- getEnclTmp
       emptyDir edir
       upgradeSchema dbh 3 tables

upgradeSchema dbh 1 tables = 
    do dbdebug "Upgrading schema 1 -> 2"
       dbdebug "Adding pcenabled column"
       run dbh "ALTER TABLE podcasts ADD pcenabled INTEGER NOT NULL DEFAULT 1" []
       dbdebug "Adding lastupdate column"
       run dbh "ALTER TABLE podcasts ADD lastupdate INTEGER" []
       setSchemaVer dbh 2
       commit dbh
       -- dbdebug "Vacuuming"
       -- run dbh "VACUUM" []
       upgradeSchema dbh 2 tables
       
upgradeSchema dbh 0 tables =
    do dbdebug "Upgrading schema 0 -> 1"
       unless ("podcasts" `elem` tables)
              (run dbh "CREATE TABLE podcasts(\
                       \castid INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT,\
                       \castname TEXT NOT NULL,\
                       \feedurl TEXT NOT NULL UNIQUE)" [] >> return ())
       unless ("episodes" `elem` tables)
              (run dbh "CREATE TABLE episodes (\
                       \castid INTEGER NOT NULL, \
                       \episodeid INTEGER NOT NULL, \
                       \title TEXT NOT NULL, \
                       \epurl TEXT NOT NULL, \
                       \enctype TEXT NOT NULL,\
                       \status TEXT NOT NULL,\
                       \UNIQUE(castid, epurl),\
                       \UNIQUE(castid, episodeid))" [] >> return ())
       setSchemaVer dbh 1
       commit dbh
       upgradeSchema dbh 1 tables

upgradeSchema dbh sv _ = 
    fail $ "Unrecognized DB schema version " ++ (show sv) ++ 
         "; you probably need a newer hpodder to read this database."

setSchemaVer :: Connection -> Integer -> IO ()
setSchemaVer dbh sv =
    do dbdebug $ "Setting schema version to " ++ show sv
       run dbh "DELETE FROM schemaver" []
       run dbh "INSERT INTO schemaver VALUES(?)" [toSql sv]
       return ()

{- | Adds a new podcast to the database.  Ignores the castid on the incoming
podcast, and returns a new object with the castid populated.

A duplicate add is an error. -}
addPodcast :: Connection -> Podcast -> IO Podcast
addPodcast dbh podcast =
    do handleSql 
        (\e -> fail $ "Error adding podcast; perhaps this URL already exists\n"
               ++ show e) $
               run dbh "INSERT INTO podcasts (castname, feedurl, pcenabled, lastupdate, lastattempt, failedattempts) VALUES (?, ?, ?, ?, ?, ?, ?)"
                         [toSql (castname podcast), toSql (feedurl podcast),
                          toSql (fromEnum (pcenabled podcast)),
                          toSql (lastupdate podcast),
                          toSql (lastattempt podcast),
                          toSql (failedattempts podcast)]
       r <- quickQuery dbh "SELECT castid FROM podcasts WHERE feedurl = ?"
            [toSql (feedurl podcast)]
       case r of
         [[x]] -> return $ podcast {castid = fromSql x}
         y -> fail $ "Unexpected result: " ++ show y

updatePodcast :: Connection -> Podcast -> IO ()
updatePodcast dbh podcast = 
    run dbh "UPDATE podcasts SET castname = ?, feedurl = ?, pcenabled = ?, \
            \lastupdate = ?, lastattempt = ?, failedattempts = ? \
            \WHERE castid = ?"
        [toSql (castname podcast), toSql (feedurl podcast),
         toSql (fromEnum (pcenabled podcast)),
         toSql (lastupdate podcast),
         toSql (lastattempt podcast),
         toSql (failedattempts podcast), toSql (castid podcast)] >> return ()

{- | Remove a podcast. -}
removePodcast :: Connection -> Podcast -> IO ()
removePodcast dbh podcast =
    do run dbh "DELETE FROM episodes WHERE castid = ?" [toSql (castid podcast)]
       run dbh "DELETE FROM podcasts WHERE castid = ?" [toSql (castid podcast)]
       return ()

getPodcasts :: Connection -> IO [Podcast]
getPodcasts dbh =
    do res <- quickQuery dbh "SELECT castid, castname, feedurl, pcenabled,\
              \lastupdate, lastattempt, failedattempts \
              \FROM podcasts ORDER BY castid" []
       return $ map podcast_convrow res

getPodcast :: Connection -> Integer -> IO [Podcast]
getPodcast dbh wantedid =
    do res <- quickQuery dbh "SELECT castid, castname, feedurl, pcenabled,\
              \lastupdate, lastattempt, failedattempts \
              \FROM podcasts WHERE castid = ? ORDER BY castid" [toSql wantedid]
       return $ map podcast_convrow res

getEpisodes :: Connection -> Podcast -> IO [Episode]
getEpisodes dbh pc =
    do r <- quickQuery dbh "SELECT episodeid, title, epurl, enctype,\
                            \status, eplength, epfirstattempt, eplastattempt, \
                            \epfailedattempts FROM episodes \
                            \WHERE castid = ? ORDER BY \
                            \episodeid" [toSql (castid pc)]
       return $ map toItem r
    where toItem [sepid, stitle, sepurl, senctype, sstatus, slength,
                  slu, sla, sfa] =
              Episode {podcast = pc, epid = fromSql sepid,
                       eptitle = fromSql stitle,
                       epurl = fromSql sepurl, eptype = fromSql senctype,
                       epstatus = read (fromSql sstatus),
                       eplength = fromSql slength,
                       epfirstattempt = fromSql slu,
                       eplastattempt = fromSql sla,
                       epfailedattempts = fromSql sfa}
          toItem x = error $ "Unexpected result in getEpisodes: " ++ show x

podcast_convrow [svid, svname, svurl, isenabled, lupdate, lattempt,
                 fattempts] =
    Podcast {castid = fromSql svid, castname = fromSql svname,
             feedurl = fromSql svurl, pcenabled = toEnum . fromSql $ isenabled,
             lastupdate = fromSql lupdate, lastattempt = fromSql lattempt,
             failedattempts = fromSql fattempts}

{- | Add a new episode.  If the episode already exists, ignore the add request
and preserve the existing record. -}
addEpisode :: Connection -> Episode -> IO Integer
addEpisode dbh ep = 
    do nextepid <- getepid
       insertEpisode "INSERT OR IGNORE" dbh ep nextepid
    where getepid = 
              do r <- quickQuery dbh "SELECT MAX(episodeid) FROM episodes WHERE castid = ?" [toSql (castid (podcast ep))]
                 case r of
                   [[SqlNull]] -> return 1
                   [[x]] -> return ((fromSql x) + (1::Int))
                   _ -> fail "Unexpected response in getepid"

{- | Update an episode.  If it doesn't already exist, create it. -}
updateEpisode :: Connection -> Episode -> IO Integer
updateEpisode dbh ep = insertEpisode "INSERT OR REPLACE" dbh ep (epid ep)

insertEpisode insertsql dbh episode newepid =
    run dbh (insertsql ++ " INTO episodes (castid, episodeid, title,\
           \epurl, enctype, status, eplength, epfirstattempt, eplastattempt,\
           \epfailedattempts) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?)")
           [toSql (castid (podcast episode)), toSql newepid, 
            toSql (eptitle episode), toSql (epurl episode), 
            toSql (eptype episode), toSql (show (epstatus episode)),
            toSql (eplength episode), toSql (epfirstattempt episode),
            toSql (eplastattempt episode), toSql (epfailedattempts episode)]

getSelectedPodcasts dbh [] = getSelectedPodcasts dbh ["all"]
getSelectedPodcasts dbh ["all"] = getPodcasts dbh
getSelectedPodcasts dbh podcastlist =
    do r <- mapM (getPodcast dbh) (map read podcastlist)
       return $ uniq $ concat r

getSelectedEpisodes :: Connection -> Podcast -> [String] -> IO [Episode]
getSelectedEpisodes _ _ [] = return []
getSelectedEpisodes dbh pc ["all"] = getEpisodes dbh pc
getSelectedEpisodes dbh pc episodelist =
    do eps <- getEpisodes dbh pc
       return $ uniq . filter (\e -> (epid e `elem` eplist)) $ eps
    where eplist = map read episodelist
