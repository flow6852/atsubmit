autocmd ExitPre * :! atsubmit stop
function! asvim#AtSubmit(...)
	let s:fname = expand("%")
	if a:0 == 0
		:! atsubmit
	       	let msg = "start"
	elseif a:0 == 1
		let msg = system("atsubmit " . a:1)
	elseif a:0 == 2 
		if a:1 == "submit" || a:2 == "test"
			let msg = system("atsubmit " . a:1 . " " . a:2 . " " . s:fname )
		else
			let msg = system("atsubmit " . a:1 . " " . a:2)
		endif
	else
		let msg = "fail: argument"
	endif	
	echo msg
endfunction
