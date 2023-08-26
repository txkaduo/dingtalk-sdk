module DingTalk.VxAPI.ErrorCode where

import ClassyPrelude


-- | 触发了当前开放接口的全局QPS限流。
apiVxEcForbiddenQpsLimitForApi :: IsString s => s
apiVxEcForbiddenQpsLimitForApi = "Forbidden.AccessDenied.QpsLimitForApi"


-- | 当前企业应用调用开放接口触发了QPS限流。
apiVxEcForbiddenQpsLimitForAppkeyAndApi :: IsString s => s
apiVxEcForbiddenQpsLimitForAppkeyAndApi = "Forbidden.AccessDenied.QpsLimitForAppkeyAndApi"


-- | 当前企业应用调用开放接口触发了QPM限流。
apiVxEcForbiddenQpmLimitForAppkeyAndApi :: IsString s => s
apiVxEcForbiddenQpmLimitForAppkeyAndApi = "Forbidden.AccessDenied.QpmLimitForAppkeyAndApi"


apiVxEcNotFound :: IsString s => s
apiVxEcNotFound  = "dentryNotExist"
