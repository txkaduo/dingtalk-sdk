module DingTalk.OAPI.ErrorCode where

import ClassyPrelude


-- | 实测表明，60121是个通用的not found错误，并不仅仅指用户找不到
oapiEcNotFound :: Int
oapiEcNotFound = 60121

oapiEcCallbackAlreadyExists :: Int
oapiEcCallbackAlreadyExists = 71006

oapiEcInvalidIp :: Int
oapiEcInvalidIp = 60020
