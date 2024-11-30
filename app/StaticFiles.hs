{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

module StaticFiles where

import Yesod.Static (staticFiles)

staticFiles "static"
