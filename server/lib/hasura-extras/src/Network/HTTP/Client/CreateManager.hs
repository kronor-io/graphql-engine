module Network.HTTP.Client.CreateManager
  ( mkHttpManager,
  )
where

import Hasura.Prelude
import Network.Connection qualified as NC
import Network.HTTP.Client qualified as HTTP
import Network.HTTP.Client.Blocklisting (Blocklist, block)
import Network.HTTP.Client.DynamicTlsPermissions qualified as HTTP
import Network.HTTP.Client.Restricted qualified as Restricted
import Control.Exception (bracket_)
import Network.Types.Extended (TlsAllow)
import System.Environment (lookupEnv, setEnv, unsetEnv)

-- | This mkHttpManager function takes a mechanism for finding the current allowlist,
-- | Thus allowing it to be coupled from any ref type such as AppStateRef.
-- | A mechanism to block IPs (both IPv4 and IPv6) has also been added to it.
mkHttpManager :: IO [TlsAllow] -> Blocklist -> IO HTTP.Manager
mkHttpManager currentAllow blocklist = do
  caCertificates <- HTTP.systemCACertificates
  let tlsSettings = HTTP.dynamicTlsSettingsWith caCertificates currentAllow
  -- One connection context, rather than one for the manager settings and one
  -- for the restricted connections. It holds a certificate store, which it
  -- doesn't use with explicit TLS settings (validation uses 'caCertificates'),
  -- but which would keep the text of the system's certificates (1.3 MiB):
  -- 'NC.initConnectionContext' reads the store from SYSTEM_CERTIFICATE_PATH if
  -- it is set, so we point it at an empty file.
  context <- withEnv "SYSTEM_CERTIFICATE_PATH" "/dev/null" NC.initConnectionContext
  HTTP.newManager
    $ Restricted.mkRestrictedManagerSettings (block blocklist) (Just context) (Just tlsSettings)

-- | Run an action with an environment variable set, restoring it afterwards.
withEnv :: String -> String -> IO a -> IO a
withEnv name value action = do
  previous <- lookupEnv name
  bracket_ (setEnv name value) (maybe (unsetEnv name) (setEnv name) previous) action
