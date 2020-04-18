" asvim
if exists('g:l_asvim')
  finish
endif
let g:l_asvim = 1

command! -nargs=* AtStart call asvim#AtStart(<f-args>) 
command! -nargs=* AtGet call asvim#AtGet(<f-args>)
command! -nargs=* AtShow call asvim#AtShow(<f-args>)
command! -nargs=* AtSubmit call asvim#AtSubmit(<f-args>)
command! -nargs=* AtTest call asvim#AtTest(<f-args>)
command! -nargs=* AtDebug call asvim#AtDebug(<f-args>)
command! -nargs=* AtLogin call asvim#AtLogin(<f-args>)
command! -nargs=* AtResult call asvim#AtResult(<f-args>)
command! -nargs=* AtLogout call asvim#AtLogout(<f-args>)
