if exists('g:l_asvim')
  finish
endif
let g:l_asvim = 1

command! -nargs=* AtStart call asvim#AtSubmit(<f-args>) 
