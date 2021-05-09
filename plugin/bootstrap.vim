let s:plugin_base = fnamemodify(expand('<sfile>'), ':p:h:h')
let s:termbuf = -1
let s:url_raw = get(g:, 'chromatin_flake_url', 'github:tek/chromatin?ref=nix')
let s:url_raw = 'path:' . s:plugin_base
let s:url = '''' . s:url_raw . ''''

function! s:close_terminal() abort "{{{
  execute 'silent! ' . s:termbuf . 'bwipeout!'
  let s:termbuf = -1
endfunction "}}}

function! s:error(error) abort "{{{
  echohl Error
  echom 'chromatin: ' . a:error
  echohl None
endfunction "}}}

function! s:job_finished(success, error, code, tempdir) abort "{{{
  if a:code == 0
    call s:close_terminal()
    call call(a:success, [])
  else
    call s:error(a:error)
  endif
endfunction "}}}

function! s:push_output(store, data) abort "{{{
  return extend(a:store, a:data)
endfunction "}}}

function! s:job(cmd, next) abort "{{{
  let output = []
  let Cb = { i, d, n -> s:push_output(output, d) }
  let opts = {
        \ 'on_stdout': Cb,
        \ 'on_stderr': Cb,
        \ 'on_exit': { i, c, e -> a:next(c, output) },
        \ 'cwd': s:plugin_base,
        \ }
  return jobstart(a:cmd, opts)
endfunction "}}}

function! s:output_job(cmd, output, next, error) abort "{{{
  let tempdir = tempname()
  call system('mkdir -p ' . tempdir)
  let output = tempdir . '/' . a:output
  call jobstart(
    \ a:cmd . ' &> ' . output,
    \ {
    \   'on_exit': { i, c, e -> s:job_finished(a:next, a:error, c, tempdir) },
    \   'cwd': s:plugin_base,
    \ },
    \ )
  sleep 100m
  execute 'belowright 15split term://tail -F ' . output
  let s:termbuf = bufnr('%')
  silent! normal! G
  silent! wincmd w
endfunction "}}}

function! ChromatinJobStderr(id, data, event) abort "{{{
  call filter(a:data, 'len(v:val) > 0')
  if len(a:data) > 0
    echom 'error in chromatin rpc job on channel ' . a:id . ': ' . string(a:data) . ' / ' . string(a:event)
  endif
endfunction "}}}

function! s:run_chromatin(built) abort "{{{
  if a:built
    echom 'chromatin: running chromatin...'
  endif
  return jobstart(
        \ 'nix run ' . s:url,
        \ {
        \   'rpc': v:true,
        \   'on_stderr': 'ChromatinJobStderr',
        \   'cwd': s:plugin_base,
        \ },
        \ )
endfunction "}}}

function! s:install_chromatin() abort "{{{
  echom 'chromatin: building chromatin...'
  return s:output_job(
    \ 'nix -L --refresh build --no-link ' . s:url,
    \ 'install-chromatin',
    \ { -> s:run_chromatin(v:true) },
    \ 'failed to install chromatin'
    \ )
endfunction "}}}

function! s:bootstrap() abort "{{{
  if !executable('nix')
    return s:error('chromatin: This plugin depends on `nix`, but it can''t be found in $PATH.')
  else
    return s:job(
      \ 'nix --refresh build --dry-run ' . s:url,
      \ { c, out -> c == 0 && empty(out) ? s:run_chromatin(v:false) : s:install_chromatin() },
      \ )
  endif
endfunction "}}}

if !get(g:, 'chromatin_disabled', 0)
  call s:bootstrap()
endif
