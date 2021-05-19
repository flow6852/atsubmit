autocmd QuitPre * call asvim#AtClose()
call setqflist([], " ", {'lines':systemlist('echo first')})
function! asvim#AtStart(...)
	copen
	call setqflist([], " ", {'nr':'$', 'lines': systemlist("atsubmit-server")})
	wincmd k
endfunction

function! asvim#AtQGet(...) "AtQGet question
	if a:0 < 1
		let cmd = "echo \"error :: command is \"AtQGet [question name] \" \""
    else
		let cmd = "atsubmit-client qget " . join(a:000, " ")
	endif
	copen
	call setqflist([], " ", {'nr':'$', 'lines': systemlist(cmd)})
	wincmd k
endfunction

function! asvim#AtCGet(...) "AtCGet contest
	if a:0 < 1
		let cmd = "echo \"error :: command is \"AtCGet [contest name] \" \""
        else
		let cmd = "atsubmit-client cget " . join(a:000, " ")
	endif
	copen
	call setqflist([], " ", {'nr':'$', 'lines': systemlist(cmd)})
	wincmd k
endfunction

function! asvim#AtShow(...) " AtShow [question]
	if a:0 == 1
		let cmd = "atsubmit-client show " . a:1
	else
		let cmd = "echo \"error :: command is \"AtShow [question name] \" \""
	endif
    let winnr = map(filter(getwininfo(),{ind, val -> val['terminal'] == 1}), {ind, val -> val['winnr']})
    if has('nvim')
        if winnr == []
            rightb vert call termopen(cmd)
        else
            execute winnr[0] . "wincmd w"
            call term_open(cmd)
        endif
    else
        if winnr == []
            rightb vert call term_start(cmd)
        else
            execute winnr[0] . "wincmd w"
            call term_start(cmd, {'curwin':1})
        endif
    endif
    let l:tex_conceal="adbmgs"
    set conceallevel=2
    setfiletype tex
    setl winfixheight
    wincmd p
	" copen
	" call setqflist([], " ", {'nr':'$', 'lines': systemlist(cmd)})
    "     let g:tex_conceal="adbmgs"
    "     set conceallevel=2
    "     setfiletype tex
	" wincmd k
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
        call delete(tmp)
endfunction

function! asvim#AtLogin(...) "AtLogin
        let txt = []
        call add(txt, inputdialog("username > "))
        call add(txt, inputdialog("password > "))
        let tmp = tempname()
        call writefile(txt, tmp)
	let cmd = "atsubmit-client login < " . tmp
	copen
	call setqflist([], " ", {'nr':'$', 'lines': systemlist(cmd)})
	wincmd k
        call delete(tmp)
endfunction

function! asvim#AtResult(...) "AtResult question
	if a:0 == 0
		let cmd = "atsubmit-client result"
        elseif a:0 == 1
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

function! asvim#AtLog(...) " AtLog
	let cmd = "atsubmit-client log"
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
        if len(getwininfo()) <= 2 && call("getbufvar", [bufnr("%"), "&buftype"]) != "quickfix"
        	!atsubmit-client stop
        	cclose
        endif
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
