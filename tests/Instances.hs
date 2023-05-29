{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-unused-imports -fno-warn-unused-matches #-}

module Instances where

import Httpstan.Model
import Httpstan.Core

import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as BL
import qualified Data.HashMap.Strict as HM
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Time as TI
import qualified Data.Vector as V
import Data.String (fromString)

import Control.Monad
import Data.Char (isSpace)
import Data.List (sort)
import Test.QuickCheck

import ApproxEq

instance Arbitrary T.Text where
  arbitrary = T.pack <$> arbitrary

instance Arbitrary TI.Day where
  arbitrary = TI.ModifiedJulianDay . (2000 +) <$> arbitrary
  shrink = (TI.ModifiedJulianDay <$>) . shrink . TI.toModifiedJulianDay

instance Arbitrary TI.UTCTime where
  arbitrary =
    TI.UTCTime <$> arbitrary <*> (TI.secondsToDiffTime <$> choose (0, 86401))

instance Arbitrary BL.ByteString where
    arbitrary = BL.pack <$> arbitrary
    shrink xs = BL.pack <$> shrink (BL.unpack xs)

instance Arbitrary ByteArray where
    arbitrary = ByteArray <$> arbitrary
    shrink (ByteArray xs) = ByteArray <$> shrink xs

instance Arbitrary Binary where
    arbitrary = Binary <$> arbitrary
    shrink (Binary xs) = Binary <$> shrink xs

instance Arbitrary DateTime where
    arbitrary = DateTime <$> arbitrary
    shrink (DateTime xs) = DateTime <$> shrink xs

instance Arbitrary Date where
    arbitrary = Date <$> arbitrary
    shrink (Date xs) = Date <$> shrink xs

#if MIN_VERSION_aeson(2,0,0)
#else
-- | A naive Arbitrary instance for A.Value:
instance Arbitrary A.Value where
  arbitrary = arbitraryValue
#endif

arbitraryValue :: Gen A.Value
arbitraryValue =
  frequency [(3, simpleTypes), (1, arrayTypes), (1, objectTypes)]
    where
      simpleTypes :: Gen A.Value
      simpleTypes =
        frequency
          [ (1, return A.Null)
          , (2, liftM A.Bool (arbitrary :: Gen Bool))
          , (2, liftM (A.Number . fromIntegral) (arbitrary :: Gen Int))
          , (2, liftM (A.String . T.pack) (arbitrary :: Gen String))
          ]
      mapF (k, v) = (fromString k, v)
      simpleAndArrays = frequency [(1, sized sizedArray), (4, simpleTypes)]
      arrayTypes = sized sizedArray
      objectTypes = sized sizedObject
      sizedArray n = liftM (A.Array . V.fromList) $ replicateM n simpleTypes
      sizedObject n =
        liftM (A.object . map mapF) $
        replicateM n $ (,) <$> (arbitrary :: Gen String) <*> simpleAndArrays

-- | Checks if a given list has no duplicates in _O(n log n)_.
hasNoDups
  :: (Ord a)
  => [a] -> Bool
hasNoDups = go Set.empty
  where
    go _ [] = True
    go s (x:xs)
      | s' <- Set.insert x s
      , Set.size s' > Set.size s = go s' xs
      | otherwise = False

instance ApproxEq TI.Day where
  (=~) = (==)

arbitraryReduced :: Arbitrary a => Int -> Gen a
arbitraryReduced n = resize (n `div` 2) arbitrary

arbitraryReducedMaybe :: Arbitrary a => Int -> Gen (Maybe a)
arbitraryReducedMaybe 0 = elements [Nothing]
arbitraryReducedMaybe n = arbitraryReduced n

arbitraryReducedMaybeValue :: Int -> Gen (Maybe A.Value)
arbitraryReducedMaybeValue 0 = elements [Nothing]
arbitraryReducedMaybeValue n = do
  generated <- arbitraryReduced n
  if generated == Just A.Null
    then return Nothing
    else return generated

-- * Models

instance Arbitrary CreateFitRequest where
  arbitrary = sized genCreateFitRequest

genCreateFitRequest :: Int -> Gen CreateFitRequest
genCreateFitRequest n =
  CreateFitRequest
    <$> arbitraryReducedMaybe n -- createFitRequestChain :: Maybe Int
    <*> arbitraryReducedMaybeValue n -- createFitRequestData :: Maybe A.Value
    <*> arbitraryReducedMaybe n -- createFitRequestDelta :: Maybe Double
    <*> arbitrary -- createFitRequestFunction :: E'Function
    <*> arbitraryReducedMaybe n -- createFitRequestGamma :: Maybe Double
    <*> arbitraryReducedMaybeValue n -- createFitRequestInit :: Maybe A.Value
    <*> arbitraryReducedMaybe n -- createFitRequestInitBuffer :: Maybe Int
    <*> arbitraryReducedMaybe n -- createFitRequestInitRadius :: Maybe Double
    <*> arbitraryReducedMaybe n -- createFitRequestKappa :: Maybe Double
    <*> arbitraryReducedMaybe n -- createFitRequestMaxDepth :: Maybe Int
    <*> arbitraryReducedMaybe n -- createFitRequestNumSamples :: Maybe Int
    <*> arbitraryReducedMaybe n -- createFitRequestNumThin :: Maybe Int
    <*> arbitraryReducedMaybe n -- createFitRequestNumWarmup :: Maybe Int
    <*> arbitraryReducedMaybe n -- createFitRequestRandomSeed :: Maybe Int
    <*> arbitraryReducedMaybe n -- createFitRequestRefresh :: Maybe Int
    <*> arbitraryReducedMaybe n -- createFitRequestSaveWarmup :: Maybe Bool
    <*> arbitraryReducedMaybe n -- createFitRequestStepsize :: Maybe Double
    <*> arbitraryReducedMaybe n -- createFitRequestStepsizeJitter :: Maybe Double
    <*> arbitraryReducedMaybe n -- createFitRequestT0 :: Maybe Double
    <*> arbitraryReducedMaybe n -- createFitRequestTermBuffer :: Maybe Int
    <*> arbitraryReducedMaybe n -- createFitRequestWindow :: Maybe Int
  
instance Arbitrary CreateModelRequest where
  arbitrary = sized genCreateModelRequest

genCreateModelRequest :: Int -> Gen CreateModelRequest
genCreateModelRequest n =
  CreateModelRequest
    <$> arbitrary -- createModelRequestProgramCode :: Text
  
instance Arbitrary Fit where
  arbitrary = sized genFit

genFit :: Int -> Gen Fit
genFit n =
  Fit
    <$> arbitrary -- fitName :: Text
  
instance Arbitrary Model where
  arbitrary = sized genModel

genModel :: Int -> Gen Model
genModel n =
  Model
    <$> arbitrary -- modelCompilerOutput :: Text
    <*> arbitrary -- modelName :: Text
    <*> arbitrary -- modelStancWarnings :: Text
  
instance Arbitrary Operation where
  arbitrary = sized genOperation

genOperation :: Int -> Gen Operation
genOperation n =
  Operation
    <$> arbitrary -- operationDone :: Bool
    <*> arbitraryReducedMaybeValue n -- operationMetadata :: Maybe A.Value
    <*> arbitrary -- operationName :: Text
    <*> arbitraryReducedMaybeValue n -- operationResult :: Maybe A.Value
  
instance Arbitrary Parameter where
  arbitrary = sized genParameter

genParameter :: Int -> Gen Parameter
genParameter n =
  Parameter
    <$> arbitrary -- parameterConstrainedNames :: [Text]
    <*> arbitrary -- parameterDims :: [Int]
    <*> arbitrary -- parameterName :: Text
  
instance Arbitrary Status where
  arbitrary = sized genStatus

genStatus :: Int -> Gen Status
genStatus n =
  Status
    <$> arbitrary -- statusCode :: Int
    <*> arbitraryReducedMaybe n -- statusDetails :: Maybe [A.Value]
    <*> arbitrary -- statusMessage :: Text
    <*> arbitrary -- statusStatus :: Text
  
instance Arbitrary V1ModelsGet200Response where
  arbitrary = sized genV1ModelsGet200Response

genV1ModelsGet200Response :: Int -> Gen V1ModelsGet200Response
genV1ModelsGet200Response n =
  V1ModelsGet200Response
    <$> arbitraryReducedMaybe n -- v1ModelsGet200ResponseModels :: Maybe [Model]
  
instance Arbitrary V1ModelsModelIdLogProbGradPost200Response where
  arbitrary = sized genV1ModelsModelIdLogProbGradPost200Response

genV1ModelsModelIdLogProbGradPost200Response :: Int -> Gen V1ModelsModelIdLogProbGradPost200Response
genV1ModelsModelIdLogProbGradPost200Response n =
  V1ModelsModelIdLogProbGradPost200Response
    <$> arbitraryReducedMaybe n -- v1ModelsModelIdLogProbGradPost200ResponseGradLogProb :: Maybe [Double]
  
instance Arbitrary V1ModelsModelIdLogProbPost200Response where
  arbitrary = sized genV1ModelsModelIdLogProbPost200Response

genV1ModelsModelIdLogProbPost200Response :: Int -> Gen V1ModelsModelIdLogProbPost200Response
genV1ModelsModelIdLogProbPost200Response n =
  V1ModelsModelIdLogProbPost200Response
    <$> arbitraryReducedMaybe n -- v1ModelsModelIdLogProbPost200ResponseLogProb :: Maybe Double
  
instance Arbitrary V1ModelsModelIdParamsPost200Response where
  arbitrary = sized genV1ModelsModelIdParamsPost200Response

genV1ModelsModelIdParamsPost200Response :: Int -> Gen V1ModelsModelIdParamsPost200Response
genV1ModelsModelIdParamsPost200Response n =
  V1ModelsModelIdParamsPost200Response
    <$> arbitraryReducedMaybe n -- v1ModelsModelIdParamsPost200ResponseId :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1ModelsModelIdParamsPost200ResponseParams :: Maybe [Parameter]
  
instance Arbitrary V1ModelsModelIdTransformInitsPost200Response where
  arbitrary = sized genV1ModelsModelIdTransformInitsPost200Response

genV1ModelsModelIdTransformInitsPost200Response :: Int -> Gen V1ModelsModelIdTransformInitsPost200Response
genV1ModelsModelIdTransformInitsPost200Response n =
  V1ModelsModelIdTransformInitsPost200Response
    <$> arbitraryReducedMaybe n -- v1ModelsModelIdTransformInitsPost200ResponseParamsRUnconstrained :: Maybe [Double]
  
instance Arbitrary V1ModelsModelIdWriteArrayPost200Response where
  arbitrary = sized genV1ModelsModelIdWriteArrayPost200Response

genV1ModelsModelIdWriteArrayPost200Response :: Int -> Gen V1ModelsModelIdWriteArrayPost200Response
genV1ModelsModelIdWriteArrayPost200Response n =
  V1ModelsModelIdWriteArrayPost200Response
    <$> arbitraryReducedMaybe n -- v1ModelsModelIdWriteArrayPost200ResponseParamsRConstrained :: Maybe [Double]
  



instance Arbitrary E'Function where
  arbitrary = arbitraryBoundedEnum

