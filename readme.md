# About

This [Neovim] plugin is a manager for plugins built in [Haskell] with [nvim-hs]
and [ribosome].
Plugins are integrated as regular Neovim packages, built with [nix] on Neovim
startup and rebuilt when they are updated.

# Install

## Neovim Style

Include *chromatin* like any other plugin using your favorite manager:

```vim
Plug 'tek/chromatin'
```

When Neovim starts for the next time, the bootstrapping mechanism will install
stack and compile *chromatin*, after which configured plugins will be loaded.
If, for example, you have:

```vim
Plug 'tek/chromatin'
Plug 'tek/proteome'
```

then [proteome] will be ready to use in the same Neovim instance.
You can rebuild plugins with `:CrmRebuild`.

## Nix Style

*chromatin* does not necessarily have to be installed as a neovim plugin; you
can run it with `nix` directly and specify plugins as flakes:

```vim
let g:chromatin_rplugins = {
  \ { 'name': 'proteome', 'spec': 'flake:github:tek/proteome' }
  \ }
call jobstart('nix --refresh run github:tek/chromatin', { 'rpc': v:true })
```

The downside of this method is that you won't get alerted that the app is being
rebuilt or that the process is writing to stderr.
As a compromise, you could copy the [bootstrap script](./plugin/bootstrap.vim)
to your nvim config and set:

```vim
let g:chromatin_flake_url = 'github:tek/chromatin'
```

## Developing Plugins

Plugins built with [ribosome] should add themselves in a `plugin/foo.vim` file
to the variable `g:chromatin_rplugins` like this:

```vim
let s:dir = fnamemodify(expand('<sfile>'), ':p:h:h')
let conf = {
      \ 'name': 'proteome',
      \ 'spec': 'flake:path:' . s:dir,
      \ 'dev': v:false,
      \ 'debug': v:false
      \ }
let g:chromatin_rplugins = get(g:, 'chromatin_rplugins', []) + [conf]
```

In order to learn how to write a plugin, check out the [ribosome]
documentation.

[Neovim]: https://github.com/neovim/neovim
[Haskell]: https://www.haskell.org
[ribosome]: https://github.com/tek/ribosome
[proteome]: https://github.com/tek/proteome
[nvim-hs]: https://github.com/neovimhaskell/nvim-hs
[nix]: https://nixos.org
