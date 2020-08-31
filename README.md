# About

This [Neovim] [Haskell] plugin is a manager for plugins built with [nvim-hs] and [ribosome].
Plugins are integrated as regular Neovim packages, built with [stack] on Neovim startup and rebuilt when they are
updated.

# Install

Include *chromatin* like any other plugin using your favorite manager:

```vim
Plug 'tek/chromatin'
```

When Neovim starts for the next time, the bootstrapping mechanism will install stack and compile *chromatin*, after
which configured plugins will be loaded.
If, for example, you have:

```vim
Plug 'tek/chromatin'
Plug 'tek/proteome'
```

then [proteome] will be ready to use in the same Neovim instance.
You can rebuild plugins with `:CrmRebuild`.

Plugins built with [ribosome] should add themselves in a `plugin/foo.vim` file to the variable `g:chromatin_rplugins`
like so:

```vim
let s:dir = fnamemodify(expand('<sfile>'), ':p:h:h')
let conf = {
      \ 'name': 'proteome',
      \ 'spec': 'stack:' . s:dir,
      \ 'dev': v:false,
      \ 'debug': v:false
      \ }
let g:chromatin_rplugins = get(g:, 'chromatin_rplugins', []) + [conf]
```

In order to learn how to write a plugin, check out the [ribosome] documentation.

[Neovim]: https://github.com/neovim/neovim
[Haskell]: https://www.haskell.org
[ribosome]: https://github.com/tek/ribosome
[proteome]: https://github.com/tek/proteome
[nvim-hs]: https://github.com/neovimhaskell/nvim-hs
[stack]: https://docs.haskellstack.org/en/stable/README
