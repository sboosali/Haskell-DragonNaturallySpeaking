module Native where

import Criterion.Main hiding (benchmark) 


benchmark message action = defaultMain [ bench message $ whnfIO action ]

