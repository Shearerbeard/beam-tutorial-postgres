{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE StandaloneDeriving   #-}

{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Main where

import           Foundation             ((<>))

import           Database.Beam
import           Database.Beam.Postgres hiding (insert, insertValues, runInsert)

import           Data.Text              (Text)


-- User Types

data UserT f
  = User
  { _userEmail     :: Columnar f Text
  , _userFirstName :: Columnar f Text
  , _userLastName  :: Columnar f Text
  , _userPassword  :: Columnar f Text }
  deriving Generic

type User = UserT Identity
type UserId = PrimaryKey UserT Identity

deriving instance Show User
deriving instance Eq User

-- User Table

instance Beamable UserT
instance Table UserT where
  data PrimaryKey UserT f = UserId (Columnar f Text) deriving Generic
  primaryKey = UserId . _userEmail
instance Beamable (PrimaryKey UserT)

-- Database

data ShoppingCartDb f = ShoppingCardDb
  { _shoppingCartUsers :: f (TableEntity UserT) }
  deriving Generic

instance Database ShoppingCartDb

shoppingCartDb :: DatabaseSettings be ShoppingCartDb
shoppingCartDb = defaultDbSettings


-- QUERIES
allUsers :: Q PgSelectSyntax ShoppingCartDb s (UserT (QExpr PgExpressionSyntax s))
allUsers = all_ (_shoppingCartUsers shoppingCartDb)

seedUsers :: Connection -> IO()
seedUsers conn = withDatabaseDebug putStrLn conn $ runInsert $
    insert (_shoppingCartUsers shoppingCartDb) $
    insertValues [ User "james@example.com" "James" "Smith" "b4cc344d25a2efe540adbf2678e2304c" {- james -}
             , User "betty@example.com" "Betty" "Jones" "82b054bd83ffad9b6cf8bdb98ce3cc2f" {- betty -}
             , User "sam@example.com" "Sam" "Taylor" "332532dcfaa1cbf61e2a266bd723612c" {- sam -} ]

selectAllUsers :: Connection -> IO()
selectAllUsers conn =
  withDatabaseDebug putStrLn conn $ do
  users <- runSelectReturningList $ select allUsers
  mapM_ (liftIO . print) users


selectSortUsersByFirstname :: Connection -> IO()
selectSortUsersByFirstname conn =
  withDatabaseDebug putStrLn conn $ do
    users <- runSelectReturningList $ select sortUsersByFirstname
    mapM_ (liftIO . print) users
  where sortUsersByFirstname = orderBy_ (\u -> (asc_ (_userFirstName u), desc_ (_userLastName u))) allUsers


boundedUsers :: Connection -> IO ()
boundedUsers conn =
  withDatabaseDebug putStrLn conn $ do
    users <- runSelectReturningList $ select boundedQuery
    mapM_ (liftIO . print) users
  where
    boundedQuery = limit_ 1 $ offset_ 1 $ orderBy_ (asc_ . _userFirstName) allUsers



main :: IO ()
main = do
  conn <- connectPostgreSQL "host=localhost dbname=beam_tutorial user=michaelshearer password=$Curl3yBrace"
  return ()
