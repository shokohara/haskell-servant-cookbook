{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
module Main where

import           Control.Monad.Logger         (NoLoggingT (..))
import           Control.Monad.Trans.Class    (lift)
import           Control.Monad.Trans.Reader   (runReaderT)
import           Control.Monad.Trans.Resource (ResourceT, runResourceT)
import           Data.Aeson
import           Data.Text                    (Text)
import qualified Data.Text                    as T
import qualified Data.Text.IO                 as T
import           Database.Persist
import           Database.Persist.Postgresql
import           Database.Persist.Sql         (SqlPersistT, runSqlConn)
import           Database.Persist.TH          (mkMigrate, mkPersist,
                                               persistLowerCase, share,
                                               sqlSettings)
import           GHC.Generics
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Servant
import           Servant.API
import           System.Environment           (getArgs)


share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
User json
    name     Text
    age      Int
    deriving Eq Show Generic
|]

type HelloAPI  = "users" :> Get '[JSON] [User]
            :<|> "users" :> Capture "name" Text :> Capture "age" Int :> Post '[JSON] ()

helloApi :: Proxy HelloAPI
helloApi = Proxy

app :: Application
app = serve helloApi server


connStr = "host=localhost dbname=test user=test password=test port=5432"

--runDB :: ConnectInfo -> SqlPersistT (ResourceT (NoLoggingT IO)) a -> IO a
runDB info = runNoLoggingT . runResourceT . withPostgresqlPool connStr 10 . runSqlConn

doMigration :: IO ()
doMigration = print "doMigration"

server :: Server HelloAPI
server = getUsers :<|> postUser
    where
        getUsers = lift selectUsers
        postUser n a = lift $ insertUser (User n a)

selectUsers :: IO [User]
selectUsers = do
    userList <- runDB connInfo $ selectList [] []
    return $ map (\(Entity _ u) -> u) userList

insertUser :: User -> IO ()
insertUser = runDB connInfo . insert_

main :: IO ()
main = do
    args <- getArgs
    let arg1 = if (length args > 0) then Just (args !! 0) else Nothing
    case arg1 of
        Just "migrate" -> doMigration
        _ -> run 8080 app
