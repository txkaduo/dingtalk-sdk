module DingTalk.VxAPI.Process where

-- {{{1 imports
import           ClassyPrelude
-- import           Control.Monad.Logger
import           Control.Monad.Except hiding (mapM_, mapM)
import           Data.Aeson           as A
import           Data.Conduit

import DingTalk.Types
import DingTalk.Helpers
import DingTalk.VxAPI.Basic

#if MIN_VERSION_classy_prelude(1, 5, 0)
import Control.Concurrent (threadDelay)
#endif
-- }}}1


maxApiVxGetProcessBatchSize :: Int
maxApiVxGetProcessBatchSize = 100


-- | 获取用户可见的审批模板
apiVxGetProcessListByUser :: HttpCallMonad env m
                          => Maybe UserId
                          -> Int
                          -> Int
                          -> ApiVxRpcWithAtk m ProcessListResponse
apiVxGetProcessListByUser m_user_id next_token batch_size =
  apiVxGetCall "v1.0" "/workflow/processes/userVisibilities/templates"
    (catMaybes
        [ "userId" &?= m_user_id
        , "nextToken" &!= next_token
        , "maxResults" &!= min maxApiVxGetProcessBatchSize batch_size
        ]
    )


apiVxSourceProcessListByUser :: HttpCallMonad env m
                             => Float  -- ^ seconds. delay between iterations
                             -> Maybe UserId
                             -> ApiVxRpcWithAtkSource m ProcessInfo
apiVxSourceProcessListByUser delay_sec m_user_id = loop 0
  where size = maxApiVxGetProcessBatchSize
        delay_us = round $ delay_sec * 1000 * 1000
        delay = liftIO $ threadDelay delay_us

        loop offset = do
          resp <- lift $ ExceptT $ apiVxGetProcessListByUser m_user_id offset size
          mapM_ yield (processListItems resp)
          mapM_ (\ x -> delay >> loop x) (processListNextCursor resp)



-- vim: set foldmethod=marker:
