{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE ViewPatterns               #-}
{-# OPTIONS_GHC
-fno-warn-unused-binds -fno-warn-unused-imports -freduction-depth=328 #-}

module Httpstan.API
  ( -- * Client and Server
    Config(..)
  , HttpstanBackend(..)
  , createHttpstanClient
  , runHttpstanServer
  , runHttpstanMiddlewareServer
  , runHttpstanClient
  , runHttpstanClientWithManager
  , callHttpstan
  , HttpstanClient
  , HttpstanClientError(..)
  -- ** Servant
  , HttpstanAPI
  -- ** Plain WAI Application
  , serverWaiApplicationHttpstan
  ) where

import           Httpstan.Types

import           Control.Monad.Catch                (Exception, MonadThrow, throwM)
import           Control.Monad.Except               (ExceptT, runExceptT)
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Reader         (ReaderT (..))
import           Data.Aeson                         (Value)
import           Data.Coerce                        (coerce)
import           Data.Data                          (Data)
import           Data.Function                      ((&))
import qualified Data.Map                           as Map
import           Data.Monoid                        ((<>))
import           Data.Proxy                         (Proxy (..))
import           Data.Set                           (Set)
import           Data.Text                          (Text)
import qualified Data.Text                          as T
import           Data.Time
import           Data.UUID                          (UUID)
import           GHC.Exts                           (IsString (..))
import           GHC.Generics                       (Generic)
import           Network.HTTP.Client                (Manager, newManager)
import           Network.HTTP.Client.TLS            (tlsManagerSettings)
import           Network.HTTP.Types.Method          (methodOptions)
import           Network.Wai                        (Middleware)
import qualified Network.Wai.Handler.Warp           as Warp
import           Servant                            (ServerError, serveWithContextT)
import           Servant.API                        hiding (addHeader)
import           Servant.API.Verbs                  (StdMethod (..), Verb)
import           Servant.Client                     (ClientEnv, Scheme (Http), ClientError, client,
                                                     mkClientEnv, parseBaseUrl)
import           Servant.Client.Core                (baseUrlPort, baseUrlHost)
import           Servant.Client.Internal.HttpClient (ClientM (..))
import           Servant.Server                     (Handler (..), Application, Context (EmptyContext))
import           Servant.Server.StaticFiles         (serveDirectoryFileServer)
import           Web.FormUrlEncoded
import           Web.HttpApiData




-- | List of elements parsed from a query.
newtype QueryList (p :: CollectionFormat) a = QueryList
  { fromQueryList :: [a]
  } deriving (Functor, Applicative, Monad, Foldable, Traversable)

-- | Formats in which a list can be encoded into a HTTP path.
data CollectionFormat
  = CommaSeparated -- ^ CSV format for multiple parameters.
  | SpaceSeparated -- ^ Also called "SSV"
  | TabSeparated -- ^ Also called "TSV"
  | PipeSeparated -- ^ `value1|value2|value2`
  | MultiParamArray -- ^ Using multiple GET parameters, e.g. `foo=bar&foo=baz`. Only for GET params.

instance FromHttpApiData a => FromHttpApiData (QueryList 'CommaSeparated a) where
  parseQueryParam = parseSeparatedQueryList ','

instance FromHttpApiData a => FromHttpApiData (QueryList 'TabSeparated a) where
  parseQueryParam = parseSeparatedQueryList '\t'

instance FromHttpApiData a => FromHttpApiData (QueryList 'SpaceSeparated a) where
  parseQueryParam = parseSeparatedQueryList ' '

instance FromHttpApiData a => FromHttpApiData (QueryList 'PipeSeparated a) where
  parseQueryParam = parseSeparatedQueryList '|'

instance FromHttpApiData a => FromHttpApiData (QueryList 'MultiParamArray a) where
  parseQueryParam = error "unimplemented FromHttpApiData for MultiParamArray collection format"

parseSeparatedQueryList :: FromHttpApiData a => Char -> Text -> Either Text (QueryList p a)
parseSeparatedQueryList char = fmap QueryList . mapM parseQueryParam . T.split (== char)

instance ToHttpApiData a => ToHttpApiData (QueryList 'CommaSeparated a) where
  toQueryParam = formatSeparatedQueryList ','

instance ToHttpApiData a => ToHttpApiData (QueryList 'TabSeparated a) where
  toQueryParam = formatSeparatedQueryList '\t'

instance ToHttpApiData a => ToHttpApiData (QueryList 'SpaceSeparated a) where
  toQueryParam = formatSeparatedQueryList ' '

instance ToHttpApiData a => ToHttpApiData (QueryList 'PipeSeparated a) where
  toQueryParam = formatSeparatedQueryList '|'

instance ToHttpApiData a => ToHttpApiData (QueryList 'MultiParamArray a) where
  toQueryParam = error "unimplemented ToHttpApiData for MultiParamArray collection format"

formatSeparatedQueryList :: ToHttpApiData a => Char ->  QueryList p a -> Text
formatSeparatedQueryList char = T.intercalate (T.singleton char) . map toQueryParam . fromQueryList


-- | Servant type-level API, generated from the OpenAPI spec for Httpstan.
type HttpstanAPI
    =    "v1" :> "health" :> Verb 'GET 200 '[JSON] NoContent -- 'v1HealthGet' route
    :<|> "v1" :> "models" :> Verb 'GET 200 '[JSON] V1ModelsGet200Response -- 'v1ModelsGet' route
    :<|> "v1" :> "models" :> Capture "model_id" Text :> Verb 'DELETE 200 '[JSON] NoContent -- 'v1ModelsModelIdDelete' route
    :<|> "v1" :> "models" :> Capture "model_id" Text :> "fits" :> Capture "fit_id" Text :> Verb 'DELETE 200 '[JSON] NoContent -- 'v1ModelsModelIdFitsFitIdDelete' route
    :<|> "v1" :> "models" :> Capture "model_id" Text :> "fits" :> Capture "fit_id" Text :> Verb 'GET 200 '[JSON] NoContent -- 'v1ModelsModelIdFitsFitIdGet' route
    :<|> "v1" :> "models" :> Capture "model_id" Text :> "fits" :> ReqBody '[JSON] CreateFitRequest :> Verb 'POST 201 '[JSON] Fit -- 'v1ModelsModelIdFitsPost' route
    :<|> "v1" :> "models" :> Capture "model_id" Text :> "log_prob_grad" :> ReqBody '[JSON] Bool :> Verb 'POST 200 '[JSON] V1ModelsModelIdLogProbGradPost200Response -- 'v1ModelsModelIdLogProbGradPost' route
    :<|> "v1" :> "models" :> Capture "model_id" Text :> "log_prob" :> ReqBody '[JSON] Bool :> Verb 'POST 200 '[JSON] V1ModelsModelIdLogProbPost200Response -- 'v1ModelsModelIdLogProbPost' route
    :<|> "v1" :> "models" :> Capture "model_id" Text :> "params" :> ReqBody '[JSON] Value :> Verb 'POST 200 '[JSON] V1ModelsModelIdParamsPost200Response -- 'v1ModelsModelIdParamsPost' route
    :<|> "v1" :> "models" :> Capture "model_id" Text :> "transform_inits" :> ReqBody '[JSON] Value :> Verb 'POST 200 '[JSON] V1ModelsModelIdTransformInitsPost200Response -- 'v1ModelsModelIdTransformInitsPost' route
    :<|> "v1" :> "models" :> Capture "model_id" Text :> "write_array" :> ReqBody '[JSON] Bool :> Verb 'POST 200 '[JSON] V1ModelsModelIdWriteArrayPost200Response -- 'v1ModelsModelIdWriteArrayPost' route
    :<|> "v1" :> "models" :> ReqBody '[JSON] CreateModelRequest :> Verb 'POST 201 '[JSON] Model -- 'v1ModelsPost' route
    :<|> "v1" :> "operations" :> Capture "operation_id" Text :> Verb 'GET 200 '[JSON] Operation -- 'v1OperationsOperationIdGet' route
    :<|> Raw


-- | Server or client configuration, specifying the host and port to query or serve on.
data Config = Config
  { configUrl :: String  -- ^ scheme://hostname:port/path, e.g. "http://localhost:8080/"
  } deriving (Eq, Ord, Show, Read)


-- | Custom exception type for our errors.
newtype HttpstanClientError = HttpstanClientError ClientError
  deriving (Show, Exception)
-- | Configuration, specifying the full url of the service.


-- | Backend for Httpstan.
-- The backend can be used both for the client and the server. The client generated from the Httpstan OpenAPI spec
-- is a backend that executes actions by sending HTTP requests (see @createHttpstanClient@). Alternatively, provided
-- a backend, the API can be served using @runHttpstanMiddlewareServer@.
data HttpstanBackend m = HttpstanBackend
  { v1HealthGet :: m NoContent{- ^ Check if service is running. -}
  , v1ModelsGet :: m V1ModelsGet200Response{- ^ List cached models. -}
  , v1ModelsModelIdDelete :: Text -> m NoContent{- ^ Delete a model which has been saved in the cache. -}
  , v1ModelsModelIdFitsFitIdDelete :: Text -> Text -> m NoContent{- ^ Delete a fit which has been saved in the cache. -}
  , v1ModelsModelIdFitsFitIdGet :: Text -> Text -> m NoContent{- ^ Result (draws, logger messages) from calling a function defined in stan::services. -}
  , v1ModelsModelIdFitsPost :: Text -> CreateFitRequest -> m Fit{- ^ A request to this endpoint starts a long-running operation. Users can retrieve information about the status of the operation by making a GET request to the operations resource endpoint. When the operation is `done`, the \"fit\" may be downloaded. (A \"fit\" collects all logger and writer messages from Stan.) ``function`` indicates the name of the ``stan::services function`` which should be called given the Stan model associated with the id ``model_id``. For example, if sampling using ``stan::services::sample::hmc_nuts_diag_e_adapt`` then ``function`` is the full function name ``stan::services::sample::hmc_nuts_diag_e_adapt``.  Sampler parameters which are not supplied will be given default values taken from CmdStan.  For example, if ``stan::services::sample::hmc_nuts_diag_e_adapt`` is the function called and the parameter ``num_samples`` is not specified, the value 1000 will be used. For a full list of default values consult the CmdStan documentation. -}
  , v1ModelsModelIdLogProbGradPost :: Text -> Bool -> m V1ModelsModelIdLogProbGradPost200Response{- ^ Returns the output of Stan C++ `stan::model::log_prob_grad`. -}
  , v1ModelsModelIdLogProbPost :: Text -> Bool -> m V1ModelsModelIdLogProbPost200Response{- ^ Returns the output of Stan C++ ``log_prob`` model class method. -}
  , v1ModelsModelIdParamsPost :: Text -> Value -> m V1ModelsModelIdParamsPost200Response{- ^ Returns the output of Stan C++ model class methods: ``constrained_param_names``, ``get_param_names`` and ``get_dims``. -}
  , v1ModelsModelIdTransformInitsPost :: Text -> Value -> m V1ModelsModelIdTransformInitsPost200Response{- ^ Returns the output of Stan C++ ``transform_inits`` model class method. -}
  , v1ModelsModelIdWriteArrayPost :: Text -> Bool -> m V1ModelsModelIdWriteArrayPost200Response{- ^ Returns the output of Stan C++ ``write_array`` model class method. -}
  , v1ModelsPost :: CreateModelRequest -> m Model{- ^ Compile a Stan model -}
  , v1OperationsOperationIdGet :: Text -> m Operation{- ^ Return Operation details. Details about an Operation include whether or not the operation is `done` and information about the progress of sampling. -}
  }


newtype HttpstanClient a = HttpstanClient
  { runClient :: ClientEnv -> ExceptT ClientError IO a
  } deriving Functor

instance Applicative HttpstanClient where
  pure x = HttpstanClient (\_ -> pure x)
  (HttpstanClient f) <*> (HttpstanClient x) =
    HttpstanClient (\env -> f env <*> x env)

instance Monad HttpstanClient where
  (HttpstanClient a) >>= f =
    HttpstanClient (\env -> do
      value <- a env
      runClient (f value) env)

instance MonadIO HttpstanClient where
  liftIO io = HttpstanClient (\_ -> liftIO io)

createHttpstanClient :: HttpstanBackend HttpstanClient
createHttpstanClient = HttpstanBackend{..}
  where
    ((coerce -> v1HealthGet) :<|>
     (coerce -> v1ModelsGet) :<|>
     (coerce -> v1ModelsModelIdDelete) :<|>
     (coerce -> v1ModelsModelIdFitsFitIdDelete) :<|>
     (coerce -> v1ModelsModelIdFitsFitIdGet) :<|>
     (coerce -> v1ModelsModelIdFitsPost) :<|>
     (coerce -> v1ModelsModelIdLogProbGradPost) :<|>
     (coerce -> v1ModelsModelIdLogProbPost) :<|>
     (coerce -> v1ModelsModelIdParamsPost) :<|>
     (coerce -> v1ModelsModelIdTransformInitsPost) :<|>
     (coerce -> v1ModelsModelIdWriteArrayPost) :<|>
     (coerce -> v1ModelsPost) :<|>
     (coerce -> v1OperationsOperationIdGet) :<|>
     _) = client (Proxy :: Proxy HttpstanAPI)

-- | Run requests in the HttpstanClient monad.
runHttpstanClient :: Config -> HttpstanClient a -> ExceptT ClientError IO a
runHttpstanClient clientConfig cl = do
  manager <- liftIO $ newManager tlsManagerSettings
  runHttpstanClientWithManager manager clientConfig cl

-- | Run requests in the HttpstanClient monad using a custom manager.
runHttpstanClientWithManager :: Manager -> Config -> HttpstanClient a -> ExceptT ClientError IO a
runHttpstanClientWithManager manager Config{..} cl = do
  url <- parseBaseUrl configUrl
  runClient cl $ mkClientEnv manager url

-- | Like @runClient@, but returns the response or throws
--   a HttpstanClientError
callHttpstan
  :: (MonadIO m, MonadThrow m)
  => ClientEnv -> HttpstanClient a -> m a
callHttpstan env f = do
  res <- liftIO $ runExceptT $ runClient f env
  case res of
    Left err       -> throwM (HttpstanClientError err)
    Right response -> pure response


requestMiddlewareId :: Application -> Application
requestMiddlewareId a = a

-- | Run the Httpstan server at the provided host and port.
runHttpstanServer
  :: (MonadIO m, MonadThrow m)
  => Config -> HttpstanBackend (ExceptT ServerError IO) -> m ()
runHttpstanServer config backend = runHttpstanMiddlewareServer config requestMiddlewareId backend

-- | Run the Httpstan server at the provided host and port.
runHttpstanMiddlewareServer
  :: (MonadIO m, MonadThrow m)
  => Config -> Middleware -> HttpstanBackend (ExceptT ServerError IO) -> m ()
runHttpstanMiddlewareServer Config{..} middleware backend = do
  url <- parseBaseUrl configUrl
  let warpSettings = Warp.defaultSettings
        & Warp.setPort (baseUrlPort url)
        & Warp.setHost (fromString $ baseUrlHost url)
  liftIO $ Warp.runSettings warpSettings $ middleware $ serverWaiApplicationHttpstan backend

-- | Plain "Network.Wai" Application for the Httpstan server.
--
-- Can be used to implement e.g. tests that call the API without a full webserver.
serverWaiApplicationHttpstan :: HttpstanBackend (ExceptT ServerError IO) -> Application
serverWaiApplicationHttpstan backend = serveWithContextT (Proxy :: Proxy HttpstanAPI) context id (serverFromBackend backend)
  where
    context = serverContext
    serverFromBackend HttpstanBackend{..} =
      (coerce v1HealthGet :<|>
       coerce v1ModelsGet :<|>
       coerce v1ModelsModelIdDelete :<|>
       coerce v1ModelsModelIdFitsFitIdDelete :<|>
       coerce v1ModelsModelIdFitsFitIdGet :<|>
       coerce v1ModelsModelIdFitsPost :<|>
       coerce v1ModelsModelIdLogProbGradPost :<|>
       coerce v1ModelsModelIdLogProbPost :<|>
       coerce v1ModelsModelIdParamsPost :<|>
       coerce v1ModelsModelIdTransformInitsPost :<|>
       coerce v1ModelsModelIdWriteArrayPost :<|>
       coerce v1ModelsPost :<|>
       coerce v1OperationsOperationIdGet :<|>
       serveDirectoryFileServer "static")


serverContext :: Context ('[])
serverContext = EmptyContext
