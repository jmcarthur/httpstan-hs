{-
   httpstan

   No description provided (generated by Openapi Generator https://github.com/openapitools/openapi-generator)

   OpenAPI Version: 3.0.1
   httpstan API version: 4.10.0
   Generated by OpenAPI Generator (https://openapi-generator.tech)
-}

{-|
Module : Httpstan.Logging
Logging functions
-}
{-# LANGUAGE CPP #-}

#ifdef USE_KATIP

module Httpstan.Logging
  ( module Httpstan.LoggingKatip
  ) where

import Httpstan.LoggingKatip

#else

module Httpstan.Logging
  ( module Httpstan.LoggingMonadLogger
  ) where

import Httpstan.LoggingMonadLogger

#endif