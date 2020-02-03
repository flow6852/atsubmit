" asvim
if exists('g:l_asvim')
  finish
endif
let g:l_asvim = 1

command! -nargs=* AtSubmit call asvim#AtSubmit(<f-args>) 
