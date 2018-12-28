let s:cache_dir = exists('$XDG_CACHE_HOME') ? $XDG_CACHE_HOME : $HOME . '/.cache'
let s:data_dir = exists('$XDG_DATA_HOME') ? $XDG_DATA_HOME : $HOME . '/.local/share'
let s:default_venvs = s:cache_dir . '/chromatin/venvs'
let s:venvs = get(g:, 'chromatin_venv_dir', s:default_venvs)
let s:venv = s:venvs . '/chromatin'
let s:plugin_base = fnamemodify(expand('<sfile>'), ':p:h:h')
let s:scripts = s:plugin_base . '/scripts'
let s:ghc_version = '8.6.3'
let s:ghc = 'ghc-' . s:ghc_version
let s:chromatin_haskell_version = '0.1.0.0'
let s:hs_base = s:data_dir . '/chromatin-hs'
let s:install_ghcup_cmd = s:scripts . '/install-ghcup'
let s:bin = $HOME . '/.ghcup/bin'
let s:ghcup_exe = s:bin . '/ghcup'
let s:ghc_exe = s:bin . '/' . s:ghc
let s:cabal_exe = s:bin . '/' . 'cabal'
let s:crm_hs_exe = $HOME . '/.cabal/bin/chromatin'
let s:termbuf = -1

function! s:close_terminal() abort "{{{
  execute 'silent! ' . s:termbuf . 'bwipeout!'
  let s:termbuf = -1
endfunction "}}}

function! s:error(error) abort "{{{
  echohl Error
  echom 'chromatin: ' . a:error
  echohl None
endfunction "}}}

function! s:job_finished(success, error, code) abort "{{{
  if a:code == 0
    call s:close_terminal()
    call call(a:success, [])
  else
    call s:error(a:error)
  endif
endfunction "}}}

function! s:job(cmd, output, next, error) abort "{{{
  let output = s:tempdir . '/' . a:output
  call jobstart(a:cmd . ' &> ' . output, { 'on_exit': { i, c, e -> s:job_finished(a:next, a:error, c) }, 'cwd': s:plugin_base })
  sleep 100m
  execute '15split term://tail -f ' . output
  let s:termbuf = bufnr('%')
  silent! wincmd w
endfunction "}}}

function! s:run_chromatin(...) abort "{{{
  if !a:0
    echom 'chromatin: running chromatin…'
  endif
  return jobstart('stack exec chromatin', { 'rpc': v:true, 'cwd': s:plugin_base })
endfunction "}}}

function! s:install_chromatin() abort "{{{
  echom 'chromatin: installing chromatin…'
  return s:job('stack build', 'install-chromatin', 's:run_chromatin', 'failed to install chromatin')
endfunction "}}}

function! s:install_stack() abort "{{{
  echom 'chromatin: installing stack…'
  return s:job(s:install_stack_cmd, 'install-stack', 's:install_chromatin', 'failed to install stack')
endfunction "}}}

function! s:push_output(store, data) abort "{{{
  return extend(a:store, a:data)
endfunction "}}}

function! s:check_executable_finished(code, output) abort "{{{
  if a:code == 0
    return index(a:output, 'Nothing to build.') >= 0 ? s:run_chromatin(1) : s:install_chromatin()
  else
    return s:error('checking project status failed: ' . string(a:output))
  endif
endfunction "}}}

function! s:check_executable() abort "{{{
  let output = []
  let Cb = { i, d, n -> s:push_output(output, d) }
  let opts = {
        \ 'on_stdout': Cb,
        \ 'on_stderr': Cb,
        \ 'on_exit': { i, c, e -> s:check_executable_finished(c, output) },
        \ 'cwd': s:plugin_base,
        \ }
  return jobstart('stack build --dry-run', opts)
endfunction "}}}

function! s:bootstrap() abort "{{{
  let s:tempdir = tempname()
  call system('mkdir ' . s:tempdir)
  if executable('stack')
    return s:check_executable()
  else
    return s:install_stack()
  endif
endfunction "}}}

if get(g:, 'chromatin_autobootstrap', 1)
  if get(g:, 'chromatin_haskell', 0) || $CHROMATIN_HASKELL != ''
    call s:bootstrap()
  endif
endif
