autocmd QuitPre * call asvim#AtClose()
call setqflist([], " ", {'lines':systemlist('echo first')})
function! asvim#AtSubmit(...)
	if a:0 == 0
		:! atsubmit
	       	let cmd = "echo start"
	elseif a:0 ==  1
		let cmd = "atsubmit " . a:1
	elseif a:0 == 2
		if a:1 == "submit" || a:1 == "test"
			let cmd = "atsubmit " . a:1 . " " . a:2 . " " . expand("%")
		else
			let cmd = "atsubmit " . a:1 . " " . a:2
		endif
	else
		let cmd = "echo fail: argument"
	endif	
	copen
	call setqflist([], " ", {'nr':'$', 'lines': systemlist(cmd)})
	wincmd k
endfunction

function! asvim#AtClose()
	:! atsubmit stop
	cclose
endfunction
