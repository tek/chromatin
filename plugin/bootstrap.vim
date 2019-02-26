let s:cache_dir = exists('$XDG_CACHE_HOME') ? $XDG_CACHE_HOME : $HOME . '/.cache'
let s:data_dir = exists('$XDG_DATA_HOME') ? $XDG_DATA_HOME : $HOME . '/.local/share'
let s:default_venvs = s:cache_dir . '/chromatin/venvs'
let s:venvs = get(g:, 'chromatin_venv_dir', s:default_venvs)
let s:venv = s:venvs . '/chromatin'
let s:plugin_base = fnamemodify(expand('<sfile>'), ':p:h:h')
let s:scripts = s:plugin_base . '/scripts'
let s:install_stack_cmd = s:scripts . '/install-stack'
let s:default_stack = $HOME . '/.local/bin/stack'
let s:termbuf = -1
let s:nvim_hs = get(g:, 'nvim_hs_vim', 0)

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

function! ChromatinJobStderr(id, data, event) abort "{{{
  call filter(a:data, 'len(v:val) > 0')
  if len(a:data) > 0
    echom 'error in chromatin rpc job on channel ' . a:id . ': ' . string(a:data) . ' / ' . string(a:event)
  endif
endfunction "}}}

function! s:run_chromatin(stack, ...) abort "{{{
  if !a:0
    echom 'chromatin: running chromatin…'
  endif
  return jobstart(a:stack . ' exec chromatin -- -v DEBUG -l/home/tek/c-log', { 'rpc': v:true, 'on_stderr': 'ChromatinJobStderr', 'cwd': s:plugin_base })
endfunction "}}}

function! s:install_chromatin(stack) abort "{{{
  echom 'chromatin: building chromatin…'
  return s:job(a:stack . ' build', 'install-chromatin', { -> s:run_chromatin(a:stack) }, 'failed to install chromatin')
endfunction "}}}

function! s:install_stack() abort "{{{
  echom 'chromatin: installing stack…'
  return s:job(s:install_stack_cmd, 'install-stack', { -> s:install_chromatin(s:default_stack) }, 'failed to install stack')
endfunction "}}}

function! s:push_output(store, data) abort "{{{
  return extend(a:store, a:data)
endfunction "}}}

function! s:project_dirty(output) abort "{{{
  return index(a:output, 'Would build:') >= 0
endfunction "}}}

function! s:check_executable_finished(stack, code, output) abort "{{{
  if a:code == 0
    return s:project_dirty(a:output) ? s:install_chromatin(a:stack) : s:run_chromatin(a:stack, 1)
  else
    return s:error('checking project status failed: ' . string(a:output))
  endif
endfunction "}}}

function! s:check_executable(stack) abort "{{{
  let output = []
  let Cb = { i, d, n -> s:push_output(output, d) }
  let opts = {
        \ 'on_stdout': Cb,
        \ 'on_stderr': Cb,
        \ 'on_exit': { i, c, e -> s:check_executable_finished(a:stack, c, output) },
        \ 'cwd': s:plugin_base,
        \ }
  return jobstart(a:stack . ' build --dry-run', opts)
endfunction "}}}

function! s:find_stack() abort "{{{
  if executable('stack')
    return 'stack'
  elseif filereadable(s:default_stack)
    return s:default_stack
  else
    return ''
  endif
endfunction "}}}

function! s:bootstrap() abort "{{{
  let s:tempdir = tempname()
  call system('mkdir ' . s:tempdir)
  let stack = s:find_stack()
  if len(stack) > 0
    return s:check_executable(stack)
  else
    return s:install_stack()
  endif
endfunction "}}}

if s:nvim_hs
  call nvimhs#start(expand('<sfile>:p:h:h'), 'chromatin', [])
elseif get(g:, 'chromatin_autobootstrap', 1)
  call s:bootstrap()
endif
