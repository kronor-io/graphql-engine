module Network.HTTP.Client.DynamicTlsPermissions
  ( dynamicTlsSettings,
    CACertificates,
    systemCACertificates,
    dynamicTlsSettingsWith,
  )
where

import Control.Exception.Safe (Exception, impureThrow)
import Data.ByteString.Char8 qualified as BC
import Data.ByteString.Short qualified as SBS
import Data.Map.Strict qualified as Map
import Data.Default.Class qualified as HTTP
import Data.X509 qualified as HTTP
import Data.X509.CertificateStore qualified as HTTP
import Data.X509.Validation qualified as HTTP
import GHC.Exception (Exception (displayException))
import Hasura.Prelude
import Network.Connection qualified as HTTP
import Network.TLS qualified as HTTP
import Network.TLS.Extra qualified as TLS
import Network.Types.Extended (TlsAllow (TlsAllow), TlsPermission (SelfSigned))
import System.X509 qualified as HTTP

newtype TlsServiceDefinitionError = TlsServiceDefinitionError
  { tlsServiceDefinitionError :: String
  }
  deriving (Show)

instance Exception TlsServiceDefinitionError where
  displayException (TlsServiceDefinitionError msg) = "TlsServiceDefinitionError: " <> show msg

errorE :: String -> c
errorE = impureThrow . TlsServiceDefinitionError

dynamicTlsSettings :: IO [TlsAllow] -> IO HTTP.TLSSettings
dynamicTlsSettings currentAllow = do
  caCertificates <- systemCACertificates
  return (dynamicTlsSettingsWith caCertificates currentAllow)

-- | The CA certificates to validate servers' certificates with: the encoding
-- of each, by the encoding of its subject.
--
-- A parsed certificate takes about 13 KiB, and until it is completely parsed,
-- it keeps its ASN.1 tokens and the text it was parsed from alive: the
-- system's 172 certificates took 5.3 MiB parsed lazily, and 2.2 MiB parsed
-- completely. Encoded, they take 0.26 MiB, in unpinned byte arrays (which can
-- be put in a compact region). Validating a server's certificate parses the CA
-- certificates of its chain, see 'chainStore'.
newtype CACertificates = CACertificates (Map.Map SBS.ShortByteString SBS.ShortByteString)

-- | The system's CA certificates, see 'CACertificates'.
systemCACertificates :: IO CACertificates
systemCACertificates = do
  store <- HTTP.getSystemCertificateStore
  let entry certificate =
        ( subjectKey $ HTTP.certSubjectDN $ HTTP.getCertificate certificate,
          SBS.toShort $ HTTP.encodeSignedObject certificate
        )
      !certificates = Map.fromList $ map entry $ HTTP.listCertificates store
  pure $ CACertificates certificates

-- | The key of a distinguished name in 'CACertificates'.
subjectKey :: HTTP.DistinguishedName -> SBS.ShortByteString
subjectKey = SBS.toShort . BC.pack . show

-- | A certificate store with the CA certificates of the issuers of the
-- certificates of a chain (and of the certificates themselves, which may be
-- CA certificates).
chainStore :: CACertificates -> HTTP.CertificateChain -> HTTP.CertificateStore
chainStore (CACertificates certificates) (HTTP.CertificateChain chain) =
  HTTP.makeCertificateStore
    [ certificate
    | signed <- chain,
      let certificate' = HTTP.getCertificate signed,
      name <- [HTTP.certIssuerDN certificate', HTTP.certSubjectDN certificate'],
      Just encoded <- [Map.lookup (subjectKey name) certificates],
      Right certificate <- [HTTP.decodeSignedCertificate (SBS.fromShort encoded)]
    ]

-- | 'dynamicTlsSettings' with the given CA certificates.
dynamicTlsSettingsWith :: CACertificates -> IO [TlsAllow] -> HTTP.TLSSettings
dynamicTlsSettingsWith caCertificates currentAllow = HTTP.TLSSettings clientParams
  where
    clientParams :: HTTP.ClientParams
    clientParams =
      (HTTP.defaultParamsClient hostName serviceIdBlob)
        { HTTP.clientSupported = HTTP.def {HTTP.supportedCiphers = TLS.ciphersuite_default}, -- supportedCiphers :: [Cipher]	Supported cipher methods. The default is empty, specify a suitable cipher list. ciphersuite_default is often a good choice.  Default: [] -- https://hackage.haskell.org/package/tls-1.5.5/docs/Network-TLS.html#t:Cipher
          -- 'certValidation' validates with 'caCertificates' instead
          HTTP.clientShared = HTTP.def {HTTP.sharedCAStore = mempty},
          HTTP.clientHooks =
            HTTP.def
              { HTTP.onServerCertificate = certValidation
              }
        }

    certValidation :: HTTP.CertificateStore -> HTTP.ValidationCache -> HTTP.ServiceID -> HTTP.CertificateChain -> IO [HTTP.FailedReason]
    certValidation _ validationCache sid chain = do
      res <- HTTP.onServerCertificate HTTP.def (chainStore caCertificates chain) validationCache sid chain
      allowList <- currentAllow
      if any (allowed sid res) allowList
        then pure []
        else pure res

    -- These always seem to be overwritten when a connection is established
    -- Should leave as errors in this case in order to validate this assumption.
    -- TODO: Is there any way to define this in terms of a pure exception?
    hostName = errorE "hostname in HTTP client defaultParamsClient accessed - this should never happen"
    serviceIdBlob = errorE "serviceIdBlob in HTTP client defaultParamsClient accessed - this should never happen"

    -- Checks that:

    allowed :: (String, BC.ByteString) -> [HTTP.FailedReason] -> TlsAllow -> Bool
    allowed (sHost, sPort) res (TlsAllow aHost aPort aPermit) =
      (sHost == aHost)
        && (BC.unpack sPort ==? aPort)
        && all (\x -> any (($ x) . permitted) (fromMaybe [SelfSigned] aPermit)) res
    -- TODO: Could clean up this check some more.

    -- Comments on failure reasons taken from https://hackage.haskell.org/package/x509-validation-1.4.7/docs/src/Data-X509-Validation.html
    -- The permitted function takes high-level concerns and translates then into certain permitted errors

    permitted SelfSigned HTTP.SelfSigned = True -- Certificate is self signed
    permitted SelfSigned (HTTP.NameMismatch _) = True -- Connection name and certificate do not match
    permitted SelfSigned HTTP.LeafNotV3 = True -- Only authorized an X509.V3 certificate as leaf certificate.
    permitted SelfSigned _ = False

    _ ==? Nothing = True
    a ==? Just a' = a == a'
