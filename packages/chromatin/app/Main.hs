import Neovim (neovim, defaultConfig, plugins)

import Chromatin.Plugin (plugin)

main :: IO ()
main =
  neovim defaultConfig {plugins = [plugin]}
