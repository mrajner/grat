map <buffer> <F5> :call AsyncCompile()<CR>
command! AsyncCompile :call AsyncCompile()<CR>
function! AsyncCompile()
  let buf = {}

  let curbuf = bufnr("%")
  bufdo let buf[bufname("%")] = bufnr("%")
  exec curbuf . "b"

  if !has_key(buf,"outputbuffer")
    e outputbuffer
    set bt=nofile
  endif

  if !has_key(buf,"errorbuffer")
    e errorbuffer
    set bt=nofile
  else
      exec buf["errorbuffer"]."bufdo %d"
  endif
  exec curbuf . "b"

  call job_start(["make"],
        \ {
        \ "out_io": "buffer",
        \ "out_name": "outputbuffer",
        \ "err_io": "buffer",
        \ "err_name": "errorbuffer",
        \ "close_cb": "MycloseHandler"
        \ })
endfunction

function! MycloseHandler(channel)
  let buf = {}
  let curbuf = bufnr("%")
  bufdo let buf[bufname("%")] = bufnr("%")
  exec curbuf . "b"
  compiler gfortran_mr
  exec "bot cbuffer " . buf["errorbuffer"]

  exec buf["errorbuffer"] ."b"
  if line('$') == 1 && getline(1) == ''
    bdelete
  endif

  exec curbuf . "b"
  redraw!
endfunction
