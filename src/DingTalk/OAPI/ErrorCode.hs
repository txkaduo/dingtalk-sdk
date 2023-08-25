module DingTalk.OAPI.ErrorCode where

import ClassyPrelude


-- | 实测表明，60121是个通用的not found错误，并不仅仅指用户找不到
oapiEcNotFound :: Int
oapiEcNotFound = 60121

oapiEcCallbackAlreadyExists :: Int
oapiEcCallbackAlreadyExists = 71006

oapiEcCallbackDoesNotExist :: Int
oapiEcCallbackDoesNotExist = 71007

oapiEcInvalidIp :: Int
oapiEcInvalidIp = 60020


-- | 钉钉平台时不时会报告调用次数太快而暂时地禁止调用
-- 一直找确认到底是真的调用太频繁，还是钉钉平台误报
-- 反正就是有可能接收到这个错误代码
-- 幸好钉钉平台只禁止调用最多一秒
oapiEcForbiddenTemporarily :: Int
oapiEcForbiddenTemporarily = 88
