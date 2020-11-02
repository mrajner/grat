" if &ft=="fortran" || &ft == "make" 
"   let b:quickrun_config = {
"         \ 'exec' : 'make -s ; bash problem.sh',
"         \ 'outputter/buffer/split' : 'botright vertical',
"         \ }
" endif
" let b:quickrun_config = {
"       \ 'exec' : 'make -f tmp.makefile',
"       \ 'outputter/buffer/split':    '%{"botright vertical" . winwidth(0)*2/5}',
"       \ }
"       " \ 'runner': 'terminal',
"       " \ }
