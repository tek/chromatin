let s:cache_dir = (exists('$XDG_CACHE_HOME') ? $XDG_CACHE_HOME : $HOME . '/.cache') . '/chromatin'
let s:ref_file = s:cache_dir . '/chromatin-ref'
let s:plugin_base = fnamemodify(expand('<sfile>'), ':p:h:h')
let s:head = s:plugin_base . '/.git/HEAD'
let s:scripts = s:plugin_base . '/scripts'
let s:install_stack_cmd = s:scripts . '/install-stack'
let s:default_stack = $HOME . '/.local/bin/stack'
let s:termbuf = -1
let s:current_ref = []
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
  call jobstart(
        \ a:cmd . ' &> ' . output,
        \ { 'on_exit': { i, c, e -> s:job_finished(a:next, a:error, c) },
        \ 'cwd': s:plugin_base },
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

function! s:store_ref() abort "{{{
  if !empty(s:current_ref)
    call writefile([s:current_ref], s:ref_file)
  endif
endfunction "}}}

function! s:run_chromatin(stack, ...) abort "{{{
  call s:store_ref()
  if !a:0
    echom 'chromatin: running chromatin…'
  endif
  return jobstart(
        \ a:stack . ' exec chromatin -- -v DEBUG -l/tmp/chromatin-run-log',
        \ { 'rpc': v:true, 'on_stderr': 'ChromatinJobStderr',
        \ 'cwd': s:plugin_base },
        \ )
endfunction "}}}

function! s:install_chromatin(stack) abort "{{{
  echom 'chromatin: building chromatin…'
  return s:job(a:stack . ' build', 'install-chromatin', { -> s:run_chromatin(a:stack) }, 'failed to install chromatin')
endfunction "}}}

function! s:install_stack() abort "{{{
  echom 'chromatin: installing stack…'
  return s:job(
        \ s:install_stack_cmd,
        \ 'install-stack',
        \ { -> s:install_chromatin(s:default_stack) },
        \ 'failed to install stack',
        \ )
endfunction "}}}

function! s:push_output(store, data) abort "{{{
  return extend(a:store, a:data)
endfunction "}}}

function! s:project_dirty(output) abort "{{{
  return match(a:output, 'Would build.*') >= 0 || match(a:output, 'No compiler found.*') >= 0
endfunction "}}}

function! s:check_executable_finished(stack, code, output) abort "{{{
  if s:project_dirty(a:output)
    return s:install_chromatin(a:stack)
  elseif a:code == 0
    return s:run_chromatin(a:stack, 1)
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
  return jobstart(a:stack . ' --no-install-ghc build --dry-run', opts)
endfunction "}}}

function! s:read_current_ref() abort "{{{
  try
    let head = filereadable(s:head) ? readfile(s:head) : ''
    let head_ref_match = matchlist(head, 'ref: \(.*\)')
    let head_ref = get(head_ref_match, 1, '')
    let repo_ref_file = s:plugin_base . '/.git/' . head_ref
    if !empty(head_ref) && filereadable(repo_ref_file)
      let s:current_ref = get(readfile(repo_ref_file), 0, '')
    endif
  catch
  endtry
endfunction "}}}

function! s:check_git_ref() abort "{{{
  try
    let stored_ref = filereadable(s:ref_file) ? get(readfile(s:ref_file), 0, '') : ''
    return !empty(s:current_ref) && stored_ref == s:current_ref
  catch
    return 0
  endtry
endfunction "}}}

function! s:run_if_git_current(stack) abort "{{{
  if s:check_git_ref()
    return s:run_chromatin(a:stack, 1)
  else
    call s:check_executable(a:stack)
  endif
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
  call mkdir(s:cache_dir, 'p')
  call s:read_current_ref()
  let s:tempdir = tempname()
  call system('mkdir ' . s:tempdir)
  let stack = s:find_stack()
  if len(stack) > 0
    return s:run_if_git_current(stack)
  else
    return s:install_stack()
  endif
endfunction "}}}

if s:nvim_hs
  call nvimhs#start(expand('<sfile>:p:h:h'), 'chromatin', [])
elseif !get(g:, 'chromatin_disabled', 0)
  call s:bootstrap()
endif
