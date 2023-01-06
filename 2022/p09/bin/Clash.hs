import Prelude

import Clash.Main (defaultMain)
import System.Environment (getArgs)

main :: IO ()
main = getArgs >>= defaultMain
