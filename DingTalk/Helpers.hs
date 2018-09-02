module DingTalk.Helpers where

-- {{{1 imports
import           ClassyPrelude
import           Control.Lens         hiding ((.=))
import           Network.Wreq

import DingTalk.Types
-- }}}1


(&=) :: ParamValue a => Text -> a -> (Text, SomeParamValue)
infix 3 &=
(&=) k v = (k, SomeParamValue v)


(&?=) :: ParamValue a => Text -> Maybe a -> Maybe (Text, SomeParamValue)
infix 3 &?=
(&?=) k v = (k &=) <$> v


type ParamKvList = [(Text, SomeParamValue)]

applyParamKvListInQs :: ParamKvList -> Options -> Options
applyParamKvListInQs kv_list opts = foldl' f opts kv_list
  where f o (k, SomeParamValue v) = o & param k .~ [ toParamValue v ]


-- vim: set foldmethod=marker:
