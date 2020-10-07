module Ohua.Backend.Convert where

import Ohua.Prelude

import Ohua.Backend.Lang


class ConvertExpr a where
    convertExpr :: TaskExpr -> a

class ConvertChannel a where
    convertChannel :: Com 'Channel -> a
