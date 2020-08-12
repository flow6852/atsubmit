autocmd QuitPre * call asvim#AtClose()
call setqflist([], " ", {'lines':systemlist('echo first')})
function! asvim#AtStart(...)
	copen
	call setqflist([], " ", {'nr':'$', 'lines': systemlist("atsubmit-server")})
	wincmd k
endfunction

function! asvim#AtQGet(...) "AtQGet question
	if a:0 != 1
		let cmd = "echo \"error :: command is \"AtQGet [question name] \" \""
        else
		let cmd = "atsubmit-client qget " . join(a, " ") # . "; echo $?"
                # let getpage = systemlist(cmd)
		# let status = str2nr(getpage[len(getpage)-1])
		# if status != 0
		# 	let cmd = "echo \" error :: " . getpage[0] . "\";echo \" status code: " . getpage[1] . "\""
		# else
                # 
		# 	echo join(split(getpage[:len(getpage)-2], "\n"), ".html ")
		# 	call system("chromium " . join(split(getpage[:len(getpage)-2]), "\n"), ".html ") . ".html &")
		# 	let cmd = "echo " . a:1
		# endif
	endif
	copen
	call setqflist([], " ", {'nr':'$', 'lines': systemlist(cmd)})
	wincmd k
endfunction

function! asvim#AtCGet(...) "AtCGet contest
	if a:0 != 1
		let cmd = "echo \"error :: command is \"AtCGet [contest name] \" \""
        else
		let cmd = "atsubmit-client cget " . join(a, " ") # . "; echo $?"
                # let getpage = systemlist(cmd)
		# let status = str2nr(getpage[len(getpage)-1])
		# if status != 0
		# 	let cmd = "echo \" error :: " . getpage[0] . "\";echo \" status code: " . getpage[1] . "\""
		# else
		# 	echo join(split(getpage[:len(getpage)-2], "\n"), ".html ")
		# 	call system("chromium " . join(split(getpage[:len(getpage)-2]), "\n"), ".html ") . ".html &")
		# 	let cmd = "echo " . a:1
		# endif
	endif
	copen
	call setqflist([], " ", {'nr':'$', 'lines': systemlist(cmd)})
	wincmd k
endfunction

function! asvim#AtShow(...) " AtGet [question]
	if a:0 == 1
		let cmd = "atsubmit-client show " . a:1
	else
		let cmd = "echo \"error :: command is \"AtShow [question name] \" \""
	endif
	copen
	call setqflist([], " ", {'nr':'$', 'lines': systemlist(cmd)})
	wincmd k
endfunction

function! asvim#AtPrint(...) " AtPrint
	if a:0 == 0
		let cmd = "atsubmit-client print"
	else
		let cmd = "echo \"error :: command is \"AtShow [question name] \" \""
	endif
	copen
	call setqflist([], " ", {'nr':'$', 'lines': systemlist(cmd)})
	wincmd k
endfunction

function! asvim#AtSubmit(...) " AtSubmit question
	if a:0 != 1
		let cmd = "echo \"error :: command is \"AtSubmit [question name] \" \""
	else 
		let cmd = "atsubmit-client submit " . a:1 . " " . expand("%")
	endif
	copen
	call setqflist([], " ", {'nr':'$', 'lines': systemlist(cmd)})
	wincmd k
endfunction

function! asvim#AtTest(...) " AtTest question
	if a:0 != 1
		let cmd = "echo \"error :: command is \"AtTest [question name] \" \""
	else 
		let cmd = "atsubmit-client test " . a:1 . " " . expand("%")
	endif
	copen
	call setqflist([], " ", {'nr':'$', 'lines': systemlist(cmd)})
	wincmd k
endfunction

function! asvim#AtDebug(...) "AtDebug
	let tmp = inputdialog("input > ")
	let txt = []
	while tmp != ""
		call add(txt, tmp)
		let tmp = inputdialog("\ninput > ")
	endwhile
	let tmp = tempname()
	call writefile(txt, tmp)
	let cmd = "atsubmit-client debug " . expand("%") . " " . tmp
	copen
	call setqflist([], " ", {'nr':'$', 'lines': systemlist(cmd)})
	wincmd k
endfunction

function! asvim#AtLogin(...) "AtLogin
        let user = inputdialog("username > ")
        let pass = inputdialog("password > ")
        call add(user, pass)
        let tmp = tempname()
        call writefile(user, tmp)
	let cmd = "atsubmit-client login < " tmp
        call delete(tmp)
	copen
	call setqflist([], " ", {'nr':'$', 'lines': systemlist(cmd)})
	wincmd k
endfunction

function! asvim#AtResult(...) "AtResult question
	if a:0 == 0
		let cmd = "atsubmit-client result"
	elseif
		let cmd = "atsubmit-client result " . a:1 
	else 
		let cmd = "echo \"error :: command is \"AtResult [ , question name] \" \""
	endif
	copen
	call setqflist([], " ", {'nr':'$', 'lines': systemlist(cmd)})
	wincmd k
endfunction

function! asvim#AtLogout(...) " AtLogout
	let cmd = "atsubmit-client logout"
	copen
	call setqflist([], " ", {'nr':'$', 'lines': systemlist(cmd)})
	wincmd k
endfunction

function! asvim#AtStop(...) " AtStop
	let cmd = "atsubmit-client stop"
	call setqflist([], " ", {'nr':'$', 'lines': systemlist(cmd)})
	cclose
endfunction

function! asvim#AtClose()
	:! atsubmit-client stop
	cclose
endfunction

" usages
" atsubmit -- start server
" atsubmit get [contest | question] -- get pages and test cases
" atsubmit show [ | question ] -- show questions or test cases
" atsubmit submit question -- sumit source for question
" atsubmit test question -- run source for test cases
" atsubmit debug question -- run source for original input 
" atsubmit login -- login and save cookie
" atsubmit result [ | contest ] -- show result about getting all or the contest
