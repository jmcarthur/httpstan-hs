{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# OPTIONS_GHC -fno-warn-unused-binds -fno-warn-unused-imports #-}

module Httpstan.Types (
  CreateFitRequest (..),
  CreateModelRequest (..),
  Fit (..),
  Model (..),
  Operation (..),
  Parameter (..),
  Status (..),
  V1ModelsGet200Response (..),
  V1ModelsModelIdLogProbGradPost200Response (..),
  V1ModelsModelIdLogProbPost200Response (..),
  V1ModelsModelIdParamsPost200Response (..),
  V1ModelsModelIdTransformInitsPost200Response (..),
  V1ModelsModelIdWriteArrayPost200Response (..),
  ) where

import Data.Data (Data)
import Data.UUID (UUID)
import Data.List (stripPrefix)
import Data.Maybe (fromMaybe)
import Data.Aeson (Value, FromJSON(..), ToJSON(..), genericToJSON, genericParseJSON)
import Data.Aeson.Types (Options(..), defaultOptions)
import Data.Set (Set)
import Data.Text (Text)
import Data.Time
import Data.Swagger (ToSchema, declareNamedSchema)
import qualified Data.Swagger as Swagger
import qualified Data.Char as Char
import qualified Data.Text as T
import qualified Data.Map as Map
import GHC.Generics (Generic)
import Data.Function ((&))


-- | 
data CreateFitRequest = CreateFitRequest
  { createFitRequestChain :: Maybe Int -- ^ 
  , createFitRequestData :: Maybe Value -- ^ 
  , createFitRequestDelta :: Maybe Double -- ^ 
  , createFitRequestFunction :: Text -- ^ 
  , createFitRequestGamma :: Maybe Double -- ^ 
  , createFitRequestInit :: Maybe Value -- ^ 
  , createFitRequestInitUnderscorebuffer :: Maybe Int -- ^ 
  , createFitRequestInitUnderscoreradius :: Maybe Double -- ^ 
  , createFitRequestKappa :: Maybe Double -- ^ 
  , createFitRequestMaxUnderscoredepth :: Maybe Int -- ^ 
  , createFitRequestNumUnderscoresamples :: Maybe Int -- ^ 
  , createFitRequestNumUnderscorethin :: Maybe Int -- ^ 
  , createFitRequestNumUnderscorewarmup :: Maybe Int -- ^ 
  , createFitRequestRandomUnderscoreseed :: Maybe Int -- ^ 
  , createFitRequestRefresh :: Maybe Int -- ^ 
  , createFitRequestSaveUnderscorewarmup :: Maybe Bool -- ^ 
  , createFitRequestStepsize :: Maybe Double -- ^ 
  , createFitRequestStepsizeUnderscorejitter :: Maybe Double -- ^ 
  , createFitRequestT0 :: Maybe Double -- ^ 
  , createFitRequestTermUnderscorebuffer :: Maybe Int -- ^ 
  , createFitRequestWindow :: Maybe Int -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON CreateFitRequest where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "createFitRequest")
instance ToJSON CreateFitRequest where
  toJSON = genericToJSON (removeFieldLabelPrefix False "createFitRequest")


-- | 
data CreateModelRequest = CreateModelRequest
  { createModelRequestProgramUnderscorecode :: Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON CreateModelRequest where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "createModelRequest")
instance ToJSON CreateModelRequest where
  toJSON = genericToJSON (removeFieldLabelPrefix False "createModelRequest")


-- | 
data Fit = Fit
  { fitName :: Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON Fit where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "fit")
instance ToJSON Fit where
  toJSON = genericToJSON (removeFieldLabelPrefix False "fit")


-- | 
data Model = Model
  { modelCompilerUnderscoreoutput :: Text -- ^ 
  , modelName :: Text -- ^ 
  , modelStancUnderscorewarnings :: Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON Model where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "model")
instance ToJSON Model where
  toJSON = genericToJSON (removeFieldLabelPrefix False "model")


-- | 
data Operation = Operation
  { operationDone :: Bool -- ^ 
  , operationMetadata :: Maybe Value -- ^ 
  , operationName :: Text -- ^ 
  , operationResult :: Maybe Value -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON Operation where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "operation")
instance ToJSON Operation where
  toJSON = genericToJSON (removeFieldLabelPrefix False "operation")


-- | 
data Parameter = Parameter
  { parameterConstrainedUnderscorenames :: [Text] -- ^ 
  , parameterDims :: [Int] -- ^ 
  , parameterName :: Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON Parameter where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "parameter")
instance ToJSON Parameter where
  toJSON = genericToJSON (removeFieldLabelPrefix False "parameter")


-- | 
data Status = Status
  { statusCode :: Int -- ^ 
  , statusDetails :: Maybe [Value] -- ^ 
  , statusMessage :: Text -- ^ 
  , statusStatus :: Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON Status where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "status")
instance ToJSON Status where
  toJSON = genericToJSON (removeFieldLabelPrefix False "status")


-- | 
data V1ModelsGet200Response = V1ModelsGet200Response
  { v1ModelsGet200ResponseModels :: Maybe [Model] -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON V1ModelsGet200Response where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "v1ModelsGet200Response")
instance ToJSON V1ModelsGet200Response where
  toJSON = genericToJSON (removeFieldLabelPrefix False "v1ModelsGet200Response")


-- | 
data V1ModelsModelIdLogProbGradPost200Response = V1ModelsModelIdLogProbGradPost200Response
  { v1ModelsModelIdLogProbGradPost200ResponseGradUnderscorelogUnderscoreprob :: Maybe [Double] -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON V1ModelsModelIdLogProbGradPost200Response where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "v1ModelsModelIdLogProbGradPost200Response")
instance ToJSON V1ModelsModelIdLogProbGradPost200Response where
  toJSON = genericToJSON (removeFieldLabelPrefix False "v1ModelsModelIdLogProbGradPost200Response")


-- | 
data V1ModelsModelIdLogProbPost200Response = V1ModelsModelIdLogProbPost200Response
  { v1ModelsModelIdLogProbPost200ResponseLogUnderscoreprob :: Maybe Double -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON V1ModelsModelIdLogProbPost200Response where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "v1ModelsModelIdLogProbPost200Response")
instance ToJSON V1ModelsModelIdLogProbPost200Response where
  toJSON = genericToJSON (removeFieldLabelPrefix False "v1ModelsModelIdLogProbPost200Response")


-- | 
data V1ModelsModelIdParamsPost200Response = V1ModelsModelIdParamsPost200Response
  { v1ModelsModelIdParamsPost200ResponseId :: Maybe Text -- ^ 
  , v1ModelsModelIdParamsPost200ResponseParams :: Maybe [Parameter] -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON V1ModelsModelIdParamsPost200Response where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "v1ModelsModelIdParamsPost200Response")
instance ToJSON V1ModelsModelIdParamsPost200Response where
  toJSON = genericToJSON (removeFieldLabelPrefix False "v1ModelsModelIdParamsPost200Response")


-- | 
data V1ModelsModelIdTransformInitsPost200Response = V1ModelsModelIdTransformInitsPost200Response
  { v1ModelsModelIdTransformInitsPost200ResponseParamsUnderscorerUnderscoreunconstrained :: Maybe [Double] -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON V1ModelsModelIdTransformInitsPost200Response where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "v1ModelsModelIdTransformInitsPost200Response")
instance ToJSON V1ModelsModelIdTransformInitsPost200Response where
  toJSON = genericToJSON (removeFieldLabelPrefix False "v1ModelsModelIdTransformInitsPost200Response")


-- | 
data V1ModelsModelIdWriteArrayPost200Response = V1ModelsModelIdWriteArrayPost200Response
  { v1ModelsModelIdWriteArrayPost200ResponseParamsUnderscorerUnderscoreconstrained :: Maybe [Double] -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON V1ModelsModelIdWriteArrayPost200Response where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "v1ModelsModelIdWriteArrayPost200Response")
instance ToJSON V1ModelsModelIdWriteArrayPost200Response where
  toJSON = genericToJSON (removeFieldLabelPrefix False "v1ModelsModelIdWriteArrayPost200Response")


uncapitalize :: String -> String
uncapitalize (first:rest) = Char.toLower first : rest
uncapitalize [] = []

-- | Remove a field label prefix during JSON parsing.
--   Also perform any replacements for special characters.
--   The @forParsing@ parameter is to distinguish between the cases in which we're using this
--   to power a @FromJSON@ or a @ToJSON@ instance. In the first case we're parsing, and we want
--   to replace special characters with their quoted equivalents (because we cannot have special
--   chars in identifier names), while we want to do vice versa when sending data instead.
removeFieldLabelPrefix :: Bool -> String -> Options
removeFieldLabelPrefix forParsing prefix =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = uncapitalize . fromMaybe (error ("did not find prefix " ++ prefix)) . stripPrefix prefix . replaceSpecialChars
    }
  where
    replaceSpecialChars field = foldl (&) field (map mkCharReplacement specialChars)
    specialChars =
      [ ("$", "'Dollar")
      , ("^", "'Caret")
      , ("|", "'Pipe")
      , ("=", "'Equal")
      , ("*", "'Star")
      , ("-", "'Dash")
      , ("&", "'Ampersand")
      , ("%", "'Percent")
      , ("#", "'Hash")
      , ("@", "'At")
      , ("!", "'Exclamation")
      , ("+", "'Plus")
      , (":", "'Colon")
      , (";", "'Semicolon")
      , (">", "'GreaterThan")
      , ("<", "'LessThan")
      , (".", "'Period")
      , ("_", "'Underscore")
      , ("?", "'Question_Mark")
      , (",", "'Comma")
      , ("'", "'Quote")
      , ("/", "'Slash")
      , ("(", "'Left_Parenthesis")
      , (")", "'Right_Parenthesis")
      , ("{", "'Left_Curly_Bracket")
      , ("}", "'Right_Curly_Bracket")
      , ("[", "'Left_Square_Bracket")
      , ("]", "'Right_Square_Bracket")
      , ("~", "'Tilde")
      , ("`", "'Backtick")
      , ("<=", "'Less_Than_Or_Equal_To")
      , (">=", "'Greater_Than_Or_Equal_To")
      , ("!=", "'Not_Equal")
      , ("<>", "'Not_Equal")
      , ("~=", "'Tilde_Equal")
      , ("\\", "'Back_Slash")
      , ("\"", "'Double_Quote")
      ]
    mkCharReplacement (replaceStr, searchStr) = T.unpack . replacer (T.pack searchStr) (T.pack replaceStr) . T.pack
    replacer =
      if forParsing
        then flip T.replace
        else T.replace
