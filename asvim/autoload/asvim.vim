autocmd QuitPre * call asvim#AtClose()
function! s:exec_cmd_term(cmd) abort
    if !exists('s:basewinid') 
        let s:basewinid = win_getid()
    endif

    if has('nvim')
        if !exists('s:exec_termid')
            bo call termopen(a:cmd, {'term_rows': '10'})
            setl winfixheight
            let s:exec_termid = win_getid()
        else
            execute win_gotoid(s:exec_termid)
            call term_open(a:cmd)
        endif
    else
        if !exists('s:exec_termid')
            bo call term_start(a:cmd, {'term_rows': '10'})
            setl winfixheight
            let s:exec_termid = win_getid()
        else
            execute win_gotoid(s:exec_termid)
            call term_start(a:cmd, {'curwin':1})
        endif
    endif

    call win_gotoid(s:basewinid)
endfunction

function! s:job_handler(ch, msg) abort
    caddexpr a:msg
endfunction

function! asvim#AtStart(...)
    let cmd = "atsubmit-server --daemonize"
    call job_start(cmd)
    call s:exec_cmd_term("echo atsubmit start")
endfunction

function! asvim#AtQGet(...) "AtQGet question
	if a:0 < 1
		let cmd = "echo \"error :: command is \"AtQGet [question name] \" \""
    else
		let cmd = "atsubmit-client qget " . join(a:000, " ")
	endif
    call s:exec_cmd_term(cmd)
endfunction

function! asvim#AtCGet(...) "AtCGet contest
	if a:0 < 1
		let cmd = "echo \"error :: command is \"AtCGet [contest name] \" \""
        else
		let cmd = "atsubmit-client cget " . join(a:000, " ")
	endif
    call s:exec_cmd_term(cmd)
endfunction

function! asvim#AtShow(...) " AtShow [question]
	if a:0 == 1
		let cmd = "atsubmit-client show " . a:1
	else
		let cmd = "echo \"error :: command is \"AtShow [question name] \" \""
	endif

    if !exists('s:basewinid')
        let s:basewinid = win_getid()
    endif
    if has('nvim')
        if !exists('s:show_termid')
            rightb vert call termopen(cmd)
            let s:show_termid = win_getid()
        else
            call win_gotoid(s:show_termid)
            call term_open(cmd)
        endif
    else
        if !exists('s:show_termid')
            rightb vert call term_start(cmd)
            let s:show_termid = win_getid()
        else
            call win_gotoid(s:show_termid)
            call term_start(cmd, {'curwin':1})
        endif
    endif
    let l:tex_conceal="adbmgs"
    set conceallevel=2
    setfiletype tex
    setl winfixheight
    call win_gotoid(s:basewinid)

    echo s:
endfunction

function! asvim#AtPrint(...) " AtPrint
	if a:0 == 0
		let cmd = "atsubmit-client print"
	else
		let cmd = "echo \"error :: command is \"AtPrint [question name] \" \""
	endif
    call s:exec_cmd_term(cmd)
endfunction

function! asvim#AtSubmit(...) " AtSubmit question
	if a:0 != 1
		let cmd = "echo \"error :: command is \"AtSubmit [question name] \" \""
	else 
		let cmd = "atsubmit-client submit " . a:1 . " " . expand("%")
	endif
    call s:exec_cmd_term(cmd)
endfunction

function! asvim#AtTest(...) " AtTest question
	if a:0 != 1
		let cmd = "echo \"error :: command is \"AtTest [question name] \" \""
	else 
		let cmd = "atsubmit-client test " . a:1 . " " . expand("%")
	endif
    call s:exec_cmd_term(cmd)
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
    call s:exec_cmd_term(cmd)
    call delete(tmp)
endfunction

function! asvim#AtLogin(...) "AtLogin
    let txt = []
    call add(txt, inputdialog("username > "))
    call add(txt, inputdialog("password > "))
    let tmp = tempname()
    call writefile(txt, tmp)
	let cmd = "atsubmit-client login < " . tmp
    call s:exec_cmd_term(cmd)
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
    call s:exec_cmd_term(cmd)
endfunction

function! asvim#AtLogout(...) " AtLogout
	let cmd = "atsubmit-client logout"
    call s:exec_cmd_term(cmd)
endfunction

function! asvim#AtLog(...) " AtLog
	let cmd = "atsubmit-client log"
    call s:exec_cmd_term(cmd)
endfunction

function! asvim#AtStop(...) " AtStop
	let cmd = "atsubmit-client stop"
    call s:exec_cmd_term(cmd)
endfunction

function! asvim#AtClose()
    if len(getwininfo()) <= 2 && call("getbufvar", [bufnr("%"), "&buftype"]) != "quickfix"
    	!atsubmit-client stop
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
