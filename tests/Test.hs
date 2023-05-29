{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Main where

import Data.Typeable (Proxy(..))
import Test.Hspec
import Test.Hspec.QuickCheck

import PropMime
import Instances ()

import Httpstan.Model
import Httpstan.MimeTypes

main :: IO ()
main =
  hspec $ modifyMaxSize (const 10) $ do
    describe "JSON instances" $ do
      pure ()
      propMimeEq MimeJSON (Proxy :: Proxy CreateFitRequest)
      propMimeEq MimeJSON (Proxy :: Proxy CreateModelRequest)
      propMimeEq MimeJSON (Proxy :: Proxy Fit)
      propMimeEq MimeJSON (Proxy :: Proxy Model)
      propMimeEq MimeJSON (Proxy :: Proxy Operation)
      propMimeEq MimeJSON (Proxy :: Proxy Parameter)
      propMimeEq MimeJSON (Proxy :: Proxy Status)
      propMimeEq MimeJSON (Proxy :: Proxy V1ModelsGet200Response)
      propMimeEq MimeJSON (Proxy :: Proxy V1ModelsModelIdLogProbGradPost200Response)
      propMimeEq MimeJSON (Proxy :: Proxy V1ModelsModelIdLogProbPost200Response)
      propMimeEq MimeJSON (Proxy :: Proxy V1ModelsModelIdParamsPost200Response)
      propMimeEq MimeJSON (Proxy :: Proxy V1ModelsModelIdTransformInitsPost200Response)
      propMimeEq MimeJSON (Proxy :: Proxy V1ModelsModelIdWriteArrayPost200Response)
      
