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
import Network.Types.Extended (TlsAllow)

-- | This mkHttpManager function takes a mechanism for finding the current allowlist,
-- | Thus allowing it to be coupled from any ref type such as AppStateRef.
-- | A mechanism to block IPs (both IPv4 and IPv6) has also been added to it.
mkHttpManager :: IO [TlsAllow] -> Blocklist -> IO HTTP.Manager
mkHttpManager currentAllow blocklist = do
  caCertificates <- HTTP.systemCACertificates
  let tlsSettings = HTTP.dynamicTlsSettingsWith caCertificates currentAllow
  -- One connection context, rather than one for the manager settings and one
  -- for the restricted connections: each reads the system certificate store,
  -- which it doesn't use with explicit TLS settings, but keeps.
  context <- NC.initConnectionContext
  HTTP.newManager
    $ Restricted.mkRestrictedManagerSettings (block blocklist) (Just context) (Just tlsSettings)
