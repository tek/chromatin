import Neovim

import Chromatin.Plugin (plugin)

main :: IO ()
main = neovim defaultConfig {plugins = [plugin]}
