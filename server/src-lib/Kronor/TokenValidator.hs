{-# LANGUAGE QuasiQuotes #-}

module Kronor.TokenValidator (startInvalidTokensListenerThread, HasInvalidTokens(..)) where

import Control.Concurrent.Extended qualified as C
import Control.Concurrent.STM qualified as STM
import Control.Immortal qualified as Immortal
import Control.Monad.Loops qualified as L
import Control.Monad.Trans.Managed (ManagedT)
import Data.Aeson
import Data.HashSet qualified as Set
import Data.UUID qualified as UUID
import Database.PG.Query qualified as PG
import Hasura.Backends.Postgres.Connection
import Hasura.Base.Error
import Hasura.Logging
import Hasura.Prelude
import Hasura.Server.Logging

class (Monad m) => HasInvalidTokens m where
  askInvalidTokens :: m (STM.TVar (Set.HashSet UUID.UUID))

data ErrorState = ErrorState
  { _esLastErrorSeen :: !(Maybe QErr)
  }
  deriving (Eq)

startInvalidTokensListenerThread ::
  C.ForkableMonadIO m =>
  Logger Hasura ->
  PG.PGPool ->
  Milliseconds ->
  STM.TVar (Set.HashSet UUID.UUID) ->
  ManagedT m (Immortal.Thread)
startInvalidTokensListenerThread logger pool interval invalidTokensRef = do
  -- Start listener thread
  listenerThread <-
    C.forkManagedT "InvalidTokens.listener" logger $
      listener logger pool invalidTokensRef interval
  logThreadStarted logger listenerThread
  pure listenerThread

logThreadStarted ::
  (MonadIO m) =>
  Logger Hasura ->
  Immortal.Thread ->
  m ()
logThreadStarted logger thread =
  let msg = "InvalidTokensListener thread started" :: Text
   in unLogger logger $
        StartupLog LevelInfo "invalid-tokens" $
          object
            [ "thread_id" .= show (Immortal.threadId thread),
              "message" .= msg
            ]

listener ::
  MonadIO m =>
  Logger Hasura ->
  PG.PGPool ->
  STM.TVar (Set.HashSet UUID.UUID) ->
  Milliseconds ->
  m void
listener logger pool invalidTokensRef interval = L.iterateM_ listenerLoop defaultErrorState
  where
    listenerLoop errorState = do
      resp <- liftIO $ invalidTokensFetcher pool invalidTokensRef

      nextErr <- case resp of
        Left respErr -> do
          if Just respErr /= _esLastErrorSeen errorState
            then do
              logError logger "could not fetch invalid tokens" respErr
              pure (ErrorState (Just respErr))
          else
              pure errorState
        Right _ -> do
          pure defaultErrorState

      liftIO $ C.sleep $ milliseconds interval
      pure nextErr

    defaultErrorState :: ErrorState
    defaultErrorState = ErrorState Nothing

invalidTokensFetcher ::
  PG.PGPool -> STM.TVar (Set.HashSet UUID.UUID) -> IO (Either QErr ())
invalidTokensFetcher pool invalidTokensRef =
  runExceptT
    ( PG.runTx pool (PG.RepeatableRead, Nothing) $
        fetchInvalidTokensFromDatabase
    )
    >>= \case
      Right rows ->
        Right <$> do
          oldMap <- STM.atomically $ STM.readTVar invalidTokensRef
          let newMap = Set.union oldMap (Set.fromList (map fst rows))
          STM.atomically $ STM.writeTVar invalidTokensRef newMap
      Left err -> pure $ Left err

fetchInvalidTokensFromDatabase :: PG.TxE QErr [(UUID.UUID, Int)]
fetchInvalidTokensFromDatabase = do
    PG.withQE
      defaultTxErrorHandler
      [PG.sql|
         SELECT token_id, 1
         FROM tenant.tokens
         WHERE blocked = true and token_type = 'backend'
      |]
      ()
      False

logError :: (MonadIO m, ToJSON a) => Logger Hasura -> Text -> a -> m ()
logError logger message err =
  unLogger logger $
    MetadataLog LevelError message $
      object ["error" .= toJSON err]