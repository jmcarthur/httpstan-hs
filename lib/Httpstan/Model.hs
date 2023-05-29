{-
   httpstan

   No description provided (generated by Openapi Generator https://github.com/openapitools/openapi-generator)

   OpenAPI Version: 3.0.1
   httpstan API version: 4.10.0
   Generated by OpenAPI Generator (https://openapi-generator.tech)
-}

{-|
Module : Httpstan.Model
-}

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-matches -fno-warn-unused-binds -fno-warn-unused-imports #-}

module Httpstan.Model where

import Httpstan.Core
import Httpstan.MimeTypes

import Data.Aeson ((.:),(.:!),(.:?),(.=))

import qualified Control.Arrow as P (left)
import qualified Data.Aeson as A
import qualified Data.ByteString as B
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import qualified Data.Data as P (Typeable, TypeRep, typeOf, typeRep)
import qualified Data.Foldable as P
import qualified Data.HashMap.Lazy as HM
import qualified Data.Map as Map
import qualified Data.Maybe as P
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Time as TI
import qualified Lens.Micro as L
import qualified Web.FormUrlEncoded as WH
import qualified Web.HttpApiData as WH

import Control.Applicative ((<|>))
import Control.Applicative (Alternative)
import Data.Function ((&))
import Data.Monoid ((<>))
import Data.Text (Text)
import Prelude (($),(/=),(.),(<$>),(<*>),(>>=),(=<<),Maybe(..),Bool(..),Char,Double,FilePath,Float,Int,Integer,String,fmap,undefined,mempty,maybe,pure,Monad,Applicative,Functor)

import qualified Prelude as P



-- * Parameter newtypes


-- ** AdjustTransform
newtype AdjustTransform = AdjustTransform { unAdjustTransform :: Bool } deriving (P.Eq, P.Show, A.ToJSON)

-- ** ConstrainedParameters
newtype ConstrainedParameters = ConstrainedParameters { unConstrainedParameters :: A.Value } deriving (P.Eq, P.Show, A.ToJSON)

-- ** FitId
newtype FitId = FitId { unFitId :: Text } deriving (P.Eq, P.Show)

-- ** IncludeGqs
newtype IncludeGqs = IncludeGqs { unIncludeGqs :: Bool } deriving (P.Eq, P.Show, A.ToJSON)

-- ** ModelId
newtype ModelId = ModelId { unModelId :: Text } deriving (P.Eq, P.Show)

-- ** OperationId
newtype OperationId = OperationId { unOperationId :: Text } deriving (P.Eq, P.Show)

-- ** ParamData
newtype ParamData = ParamData { unParamData :: A.Value } deriving (P.Eq, P.Show, A.ToJSON)

-- * Models


-- ** CreateFitRequest
-- | CreateFitRequest
data CreateFitRequest = CreateFitRequest
  { createFitRequestChain :: !(Maybe Int) -- ^ "chain"
  , createFitRequestData :: !(Maybe A.Value) -- ^ "data"
  , createFitRequestDelta :: !(Maybe Double) -- ^ "delta"
  , createFitRequestFunction :: !(E'Function) -- ^ /Required/ "function"
  , createFitRequestGamma :: !(Maybe Double) -- ^ "gamma"
  , createFitRequestInit :: !(Maybe A.Value) -- ^ "init"
  , createFitRequestInitBuffer :: !(Maybe Int) -- ^ "init_buffer"
  , createFitRequestInitRadius :: !(Maybe Double) -- ^ "init_radius"
  , createFitRequestKappa :: !(Maybe Double) -- ^ "kappa"
  , createFitRequestMaxDepth :: !(Maybe Int) -- ^ "max_depth"
  , createFitRequestNumSamples :: !(Maybe Int) -- ^ "num_samples"
  , createFitRequestNumThin :: !(Maybe Int) -- ^ "num_thin"
  , createFitRequestNumWarmup :: !(Maybe Int) -- ^ "num_warmup"
  , createFitRequestRandomSeed :: !(Maybe Int) -- ^ "random_seed"
  , createFitRequestRefresh :: !(Maybe Int) -- ^ "refresh"
  , createFitRequestSaveWarmup :: !(Maybe Bool) -- ^ "save_warmup"
  , createFitRequestStepsize :: !(Maybe Double) -- ^ "stepsize"
  , createFitRequestStepsizeJitter :: !(Maybe Double) -- ^ "stepsize_jitter"
  , createFitRequestT0 :: !(Maybe Double) -- ^ "t0"
  , createFitRequestTermBuffer :: !(Maybe Int) -- ^ "term_buffer"
  , createFitRequestWindow :: !(Maybe Int) -- ^ "window"
  } deriving (P.Show, P.Eq, P.Typeable)

-- | FromJSON CreateFitRequest
instance A.FromJSON CreateFitRequest where
  parseJSON = A.withObject "CreateFitRequest" $ \o ->
    CreateFitRequest
      <$> (o .:? "chain")
      <*> (o .:? "data")
      <*> (o .:? "delta")
      <*> (o .:  "function")
      <*> (o .:? "gamma")
      <*> (o .:? "init")
      <*> (o .:? "init_buffer")
      <*> (o .:? "init_radius")
      <*> (o .:? "kappa")
      <*> (o .:? "max_depth")
      <*> (o .:? "num_samples")
      <*> (o .:? "num_thin")
      <*> (o .:? "num_warmup")
      <*> (o .:? "random_seed")
      <*> (o .:? "refresh")
      <*> (o .:? "save_warmup")
      <*> (o .:? "stepsize")
      <*> (o .:? "stepsize_jitter")
      <*> (o .:? "t0")
      <*> (o .:? "term_buffer")
      <*> (o .:? "window")

-- | ToJSON CreateFitRequest
instance A.ToJSON CreateFitRequest where
  toJSON CreateFitRequest {..} =
   _omitNulls
      [ "chain" .= createFitRequestChain
      , "data" .= createFitRequestData
      , "delta" .= createFitRequestDelta
      , "function" .= createFitRequestFunction
      , "gamma" .= createFitRequestGamma
      , "init" .= createFitRequestInit
      , "init_buffer" .= createFitRequestInitBuffer
      , "init_radius" .= createFitRequestInitRadius
      , "kappa" .= createFitRequestKappa
      , "max_depth" .= createFitRequestMaxDepth
      , "num_samples" .= createFitRequestNumSamples
      , "num_thin" .= createFitRequestNumThin
      , "num_warmup" .= createFitRequestNumWarmup
      , "random_seed" .= createFitRequestRandomSeed
      , "refresh" .= createFitRequestRefresh
      , "save_warmup" .= createFitRequestSaveWarmup
      , "stepsize" .= createFitRequestStepsize
      , "stepsize_jitter" .= createFitRequestStepsizeJitter
      , "t0" .= createFitRequestT0
      , "term_buffer" .= createFitRequestTermBuffer
      , "window" .= createFitRequestWindow
      ]


-- | Construct a value of type 'CreateFitRequest' (by applying it's required fields, if any)
mkCreateFitRequest
  :: E'Function -- ^ 'createFitRequestFunction' 
  -> CreateFitRequest
mkCreateFitRequest createFitRequestFunction =
  CreateFitRequest
  { createFitRequestChain = Nothing
  , createFitRequestData = Nothing
  , createFitRequestDelta = Nothing
  , createFitRequestFunction
  , createFitRequestGamma = Nothing
  , createFitRequestInit = Nothing
  , createFitRequestInitBuffer = Nothing
  , createFitRequestInitRadius = Nothing
  , createFitRequestKappa = Nothing
  , createFitRequestMaxDepth = Nothing
  , createFitRequestNumSamples = Nothing
  , createFitRequestNumThin = Nothing
  , createFitRequestNumWarmup = Nothing
  , createFitRequestRandomSeed = Nothing
  , createFitRequestRefresh = Nothing
  , createFitRequestSaveWarmup = Nothing
  , createFitRequestStepsize = Nothing
  , createFitRequestStepsizeJitter = Nothing
  , createFitRequestT0 = Nothing
  , createFitRequestTermBuffer = Nothing
  , createFitRequestWindow = Nothing
  }

-- ** CreateModelRequest
-- | CreateModelRequest
data CreateModelRequest = CreateModelRequest
  { createModelRequestProgramCode :: !(Text) -- ^ /Required/ "program_code"
  } deriving (P.Show, P.Eq, P.Typeable)

-- | FromJSON CreateModelRequest
instance A.FromJSON CreateModelRequest where
  parseJSON = A.withObject "CreateModelRequest" $ \o ->
    CreateModelRequest
      <$> (o .:  "program_code")

-- | ToJSON CreateModelRequest
instance A.ToJSON CreateModelRequest where
  toJSON CreateModelRequest {..} =
   _omitNulls
      [ "program_code" .= createModelRequestProgramCode
      ]


-- | Construct a value of type 'CreateModelRequest' (by applying it's required fields, if any)
mkCreateModelRequest
  :: Text -- ^ 'createModelRequestProgramCode' 
  -> CreateModelRequest
mkCreateModelRequest createModelRequestProgramCode =
  CreateModelRequest
  { createModelRequestProgramCode
  }

-- ** Fit
-- | Fit
data Fit = Fit
  { fitName :: !(Text) -- ^ /Required/ "name"
  } deriving (P.Show, P.Eq, P.Typeable)

-- | FromJSON Fit
instance A.FromJSON Fit where
  parseJSON = A.withObject "Fit" $ \o ->
    Fit
      <$> (o .:  "name")

-- | ToJSON Fit
instance A.ToJSON Fit where
  toJSON Fit {..} =
   _omitNulls
      [ "name" .= fitName
      ]


-- | Construct a value of type 'Fit' (by applying it's required fields, if any)
mkFit
  :: Text -- ^ 'fitName' 
  -> Fit
mkFit fitName =
  Fit
  { fitName
  }

-- ** Model
-- | Model
data Model = Model
  { modelCompilerOutput :: !(Text) -- ^ /Required/ "compiler_output"
  , modelName :: !(Text) -- ^ /Required/ "name"
  , modelStancWarnings :: !(Text) -- ^ /Required/ "stanc_warnings"
  } deriving (P.Show, P.Eq, P.Typeable)

-- | FromJSON Model
instance A.FromJSON Model where
  parseJSON = A.withObject "Model" $ \o ->
    Model
      <$> (o .:  "compiler_output")
      <*> (o .:  "name")
      <*> (o .:  "stanc_warnings")

-- | ToJSON Model
instance A.ToJSON Model where
  toJSON Model {..} =
   _omitNulls
      [ "compiler_output" .= modelCompilerOutput
      , "name" .= modelName
      , "stanc_warnings" .= modelStancWarnings
      ]


-- | Construct a value of type 'Model' (by applying it's required fields, if any)
mkModel
  :: Text -- ^ 'modelCompilerOutput' 
  -> Text -- ^ 'modelName' 
  -> Text -- ^ 'modelStancWarnings' 
  -> Model
mkModel modelCompilerOutput modelName modelStancWarnings =
  Model
  { modelCompilerOutput
  , modelName
  , modelStancWarnings
  }

-- ** Operation
-- | Operation
data Operation = Operation
  { operationDone :: !(Bool) -- ^ /Required/ "done"
  , operationMetadata :: !(Maybe A.Value) -- ^ "metadata"
  , operationName :: !(Text) -- ^ /Required/ "name"
  , operationResult :: !(Maybe A.Value) -- ^ "result"
  } deriving (P.Show, P.Eq, P.Typeable)

-- | FromJSON Operation
instance A.FromJSON Operation where
  parseJSON = A.withObject "Operation" $ \o ->
    Operation
      <$> (o .:  "done")
      <*> (o .:? "metadata")
      <*> (o .:  "name")
      <*> (o .:? "result")

-- | ToJSON Operation
instance A.ToJSON Operation where
  toJSON Operation {..} =
   _omitNulls
      [ "done" .= operationDone
      , "metadata" .= operationMetadata
      , "name" .= operationName
      , "result" .= operationResult
      ]


-- | Construct a value of type 'Operation' (by applying it's required fields, if any)
mkOperation
  :: Bool -- ^ 'operationDone' 
  -> Text -- ^ 'operationName' 
  -> Operation
mkOperation operationDone operationName =
  Operation
  { operationDone
  , operationMetadata = Nothing
  , operationName
  , operationResult = Nothing
  }

-- ** Parameter
-- | Parameter
data Parameter = Parameter
  { parameterConstrainedNames :: !([Text]) -- ^ /Required/ "constrained_names"
  , parameterDims :: !([Int]) -- ^ /Required/ "dims"
  , parameterName :: !(Text) -- ^ /Required/ "name"
  } deriving (P.Show, P.Eq, P.Typeable)

-- | FromJSON Parameter
instance A.FromJSON Parameter where
  parseJSON = A.withObject "Parameter" $ \o ->
    Parameter
      <$> (o .:  "constrained_names")
      <*> (o .:  "dims")
      <*> (o .:  "name")

-- | ToJSON Parameter
instance A.ToJSON Parameter where
  toJSON Parameter {..} =
   _omitNulls
      [ "constrained_names" .= parameterConstrainedNames
      , "dims" .= parameterDims
      , "name" .= parameterName
      ]


-- | Construct a value of type 'Parameter' (by applying it's required fields, if any)
mkParameter
  :: [Text] -- ^ 'parameterConstrainedNames' 
  -> [Int] -- ^ 'parameterDims' 
  -> Text -- ^ 'parameterName' 
  -> Parameter
mkParameter parameterConstrainedNames parameterDims parameterName =
  Parameter
  { parameterConstrainedNames
  , parameterDims
  , parameterName
  }

-- ** Status
-- | Status
data Status = Status
  { statusCode :: !(Int) -- ^ /Required/ "code"
  , statusDetails :: !(Maybe [A.Value]) -- ^ "details"
  , statusMessage :: !(Text) -- ^ /Required/ "message"
  , statusStatus :: !(Text) -- ^ /Required/ "status"
  } deriving (P.Show, P.Eq, P.Typeable)

-- | FromJSON Status
instance A.FromJSON Status where
  parseJSON = A.withObject "Status" $ \o ->
    Status
      <$> (o .:  "code")
      <*> (o .:? "details")
      <*> (o .:  "message")
      <*> (o .:  "status")

-- | ToJSON Status
instance A.ToJSON Status where
  toJSON Status {..} =
   _omitNulls
      [ "code" .= statusCode
      , "details" .= statusDetails
      , "message" .= statusMessage
      , "status" .= statusStatus
      ]


-- | Construct a value of type 'Status' (by applying it's required fields, if any)
mkStatus
  :: Int -- ^ 'statusCode' 
  -> Text -- ^ 'statusMessage' 
  -> Text -- ^ 'statusStatus' 
  -> Status
mkStatus statusCode statusMessage statusStatus =
  Status
  { statusCode
  , statusDetails = Nothing
  , statusMessage
  , statusStatus
  }

-- ** V1ModelsGet200Response
-- | V1ModelsGet200Response
data V1ModelsGet200Response = V1ModelsGet200Response
  { v1ModelsGet200ResponseModels :: !(Maybe [Model]) -- ^ "models"
  } deriving (P.Show, P.Eq, P.Typeable)

-- | FromJSON V1ModelsGet200Response
instance A.FromJSON V1ModelsGet200Response where
  parseJSON = A.withObject "V1ModelsGet200Response" $ \o ->
    V1ModelsGet200Response
      <$> (o .:? "models")

-- | ToJSON V1ModelsGet200Response
instance A.ToJSON V1ModelsGet200Response where
  toJSON V1ModelsGet200Response {..} =
   _omitNulls
      [ "models" .= v1ModelsGet200ResponseModels
      ]


-- | Construct a value of type 'V1ModelsGet200Response' (by applying it's required fields, if any)
mkV1ModelsGet200Response
  :: V1ModelsGet200Response
mkV1ModelsGet200Response =
  V1ModelsGet200Response
  { v1ModelsGet200ResponseModels = Nothing
  }

-- ** V1ModelsModelIdLogProbGradPost200Response
-- | V1ModelsModelIdLogProbGradPost200Response
data V1ModelsModelIdLogProbGradPost200Response = V1ModelsModelIdLogProbGradPost200Response
  { v1ModelsModelIdLogProbGradPost200ResponseGradLogProb :: !(Maybe [Double]) -- ^ "grad_log_prob"
  } deriving (P.Show, P.Eq, P.Typeable)

-- | FromJSON V1ModelsModelIdLogProbGradPost200Response
instance A.FromJSON V1ModelsModelIdLogProbGradPost200Response where
  parseJSON = A.withObject "V1ModelsModelIdLogProbGradPost200Response" $ \o ->
    V1ModelsModelIdLogProbGradPost200Response
      <$> (o .:? "grad_log_prob")

-- | ToJSON V1ModelsModelIdLogProbGradPost200Response
instance A.ToJSON V1ModelsModelIdLogProbGradPost200Response where
  toJSON V1ModelsModelIdLogProbGradPost200Response {..} =
   _omitNulls
      [ "grad_log_prob" .= v1ModelsModelIdLogProbGradPost200ResponseGradLogProb
      ]


-- | Construct a value of type 'V1ModelsModelIdLogProbGradPost200Response' (by applying it's required fields, if any)
mkV1ModelsModelIdLogProbGradPost200Response
  :: V1ModelsModelIdLogProbGradPost200Response
mkV1ModelsModelIdLogProbGradPost200Response =
  V1ModelsModelIdLogProbGradPost200Response
  { v1ModelsModelIdLogProbGradPost200ResponseGradLogProb = Nothing
  }

-- ** V1ModelsModelIdLogProbPost200Response
-- | V1ModelsModelIdLogProbPost200Response
data V1ModelsModelIdLogProbPost200Response = V1ModelsModelIdLogProbPost200Response
  { v1ModelsModelIdLogProbPost200ResponseLogProb :: !(Maybe Double) -- ^ "log_prob"
  } deriving (P.Show, P.Eq, P.Typeable)

-- | FromJSON V1ModelsModelIdLogProbPost200Response
instance A.FromJSON V1ModelsModelIdLogProbPost200Response where
  parseJSON = A.withObject "V1ModelsModelIdLogProbPost200Response" $ \o ->
    V1ModelsModelIdLogProbPost200Response
      <$> (o .:? "log_prob")

-- | ToJSON V1ModelsModelIdLogProbPost200Response
instance A.ToJSON V1ModelsModelIdLogProbPost200Response where
  toJSON V1ModelsModelIdLogProbPost200Response {..} =
   _omitNulls
      [ "log_prob" .= v1ModelsModelIdLogProbPost200ResponseLogProb
      ]


-- | Construct a value of type 'V1ModelsModelIdLogProbPost200Response' (by applying it's required fields, if any)
mkV1ModelsModelIdLogProbPost200Response
  :: V1ModelsModelIdLogProbPost200Response
mkV1ModelsModelIdLogProbPost200Response =
  V1ModelsModelIdLogProbPost200Response
  { v1ModelsModelIdLogProbPost200ResponseLogProb = Nothing
  }

-- ** V1ModelsModelIdParamsPost200Response
-- | V1ModelsModelIdParamsPost200Response
data V1ModelsModelIdParamsPost200Response = V1ModelsModelIdParamsPost200Response
  { v1ModelsModelIdParamsPost200ResponseId :: !(Maybe Text) -- ^ "id"
  , v1ModelsModelIdParamsPost200ResponseParams :: !(Maybe [Parameter]) -- ^ "params"
  } deriving (P.Show, P.Eq, P.Typeable)

-- | FromJSON V1ModelsModelIdParamsPost200Response
instance A.FromJSON V1ModelsModelIdParamsPost200Response where
  parseJSON = A.withObject "V1ModelsModelIdParamsPost200Response" $ \o ->
    V1ModelsModelIdParamsPost200Response
      <$> (o .:? "id")
      <*> (o .:? "params")

-- | ToJSON V1ModelsModelIdParamsPost200Response
instance A.ToJSON V1ModelsModelIdParamsPost200Response where
  toJSON V1ModelsModelIdParamsPost200Response {..} =
   _omitNulls
      [ "id" .= v1ModelsModelIdParamsPost200ResponseId
      , "params" .= v1ModelsModelIdParamsPost200ResponseParams
      ]


-- | Construct a value of type 'V1ModelsModelIdParamsPost200Response' (by applying it's required fields, if any)
mkV1ModelsModelIdParamsPost200Response
  :: V1ModelsModelIdParamsPost200Response
mkV1ModelsModelIdParamsPost200Response =
  V1ModelsModelIdParamsPost200Response
  { v1ModelsModelIdParamsPost200ResponseId = Nothing
  , v1ModelsModelIdParamsPost200ResponseParams = Nothing
  }

-- ** V1ModelsModelIdTransformInitsPost200Response
-- | V1ModelsModelIdTransformInitsPost200Response
data V1ModelsModelIdTransformInitsPost200Response = V1ModelsModelIdTransformInitsPost200Response
  { v1ModelsModelIdTransformInitsPost200ResponseParamsRUnconstrained :: !(Maybe [Double]) -- ^ "params_r_unconstrained"
  } deriving (P.Show, P.Eq, P.Typeable)

-- | FromJSON V1ModelsModelIdTransformInitsPost200Response
instance A.FromJSON V1ModelsModelIdTransformInitsPost200Response where
  parseJSON = A.withObject "V1ModelsModelIdTransformInitsPost200Response" $ \o ->
    V1ModelsModelIdTransformInitsPost200Response
      <$> (o .:? "params_r_unconstrained")

-- | ToJSON V1ModelsModelIdTransformInitsPost200Response
instance A.ToJSON V1ModelsModelIdTransformInitsPost200Response where
  toJSON V1ModelsModelIdTransformInitsPost200Response {..} =
   _omitNulls
      [ "params_r_unconstrained" .= v1ModelsModelIdTransformInitsPost200ResponseParamsRUnconstrained
      ]


-- | Construct a value of type 'V1ModelsModelIdTransformInitsPost200Response' (by applying it's required fields, if any)
mkV1ModelsModelIdTransformInitsPost200Response
  :: V1ModelsModelIdTransformInitsPost200Response
mkV1ModelsModelIdTransformInitsPost200Response =
  V1ModelsModelIdTransformInitsPost200Response
  { v1ModelsModelIdTransformInitsPost200ResponseParamsRUnconstrained = Nothing
  }

-- ** V1ModelsModelIdWriteArrayPost200Response
-- | V1ModelsModelIdWriteArrayPost200Response
data V1ModelsModelIdWriteArrayPost200Response = V1ModelsModelIdWriteArrayPost200Response
  { v1ModelsModelIdWriteArrayPost200ResponseParamsRConstrained :: !(Maybe [Double]) -- ^ "params_r_constrained"
  } deriving (P.Show, P.Eq, P.Typeable)

-- | FromJSON V1ModelsModelIdWriteArrayPost200Response
instance A.FromJSON V1ModelsModelIdWriteArrayPost200Response where
  parseJSON = A.withObject "V1ModelsModelIdWriteArrayPost200Response" $ \o ->
    V1ModelsModelIdWriteArrayPost200Response
      <$> (o .:? "params_r_constrained")

-- | ToJSON V1ModelsModelIdWriteArrayPost200Response
instance A.ToJSON V1ModelsModelIdWriteArrayPost200Response where
  toJSON V1ModelsModelIdWriteArrayPost200Response {..} =
   _omitNulls
      [ "params_r_constrained" .= v1ModelsModelIdWriteArrayPost200ResponseParamsRConstrained
      ]


-- | Construct a value of type 'V1ModelsModelIdWriteArrayPost200Response' (by applying it's required fields, if any)
mkV1ModelsModelIdWriteArrayPost200Response
  :: V1ModelsModelIdWriteArrayPost200Response
mkV1ModelsModelIdWriteArrayPost200Response =
  V1ModelsModelIdWriteArrayPost200Response
  { v1ModelsModelIdWriteArrayPost200ResponseParamsRConstrained = Nothing
  }


-- * Enums


-- ** E'Function

-- | Enum of 'Text'
data E'Function
  = E'Function'Hmc_nuts_diag_e_adapt -- ^ @"stan::services::sample::hmc_nuts_diag_e_adapt"@
  | E'Function'Fixed_param -- ^ @"stan::services::sample::fixed_param"@
  deriving (P.Show, P.Eq, P.Typeable, P.Ord, P.Bounded, P.Enum)

instance A.ToJSON E'Function where toJSON = A.toJSON . fromE'Function
instance A.FromJSON E'Function where parseJSON o = P.either P.fail (pure . P.id) . toE'Function =<< A.parseJSON o
instance WH.ToHttpApiData E'Function where toQueryParam = WH.toQueryParam . fromE'Function
instance WH.FromHttpApiData E'Function where parseQueryParam o = WH.parseQueryParam o >>= P.left T.pack . toE'Function
instance MimeRender MimeMultipartFormData E'Function where mimeRender _ = mimeRenderDefaultMultipartFormData

-- | unwrap 'E'Function' enum
fromE'Function :: E'Function -> Text
fromE'Function = \case
  E'Function'Hmc_nuts_diag_e_adapt -> "stan::services::sample::hmc_nuts_diag_e_adapt"
  E'Function'Fixed_param -> "stan::services::sample::fixed_param"

-- | parse 'E'Function' enum
toE'Function :: Text -> P.Either String E'Function
toE'Function = \case
  "stan::services::sample::hmc_nuts_diag_e_adapt" -> P.Right E'Function'Hmc_nuts_diag_e_adapt
  "stan::services::sample::fixed_param" -> P.Right E'Function'Fixed_param
  s -> P.Left $ "toE'Function: enum parse failure: " P.++ P.show s


