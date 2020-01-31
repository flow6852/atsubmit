# atsubmit

```bash
atsubmit # start atsubmit server
atsubmit login # relogin and get cookie [ERROR] canot input (remove?)
atsubmit get [question name] # getting latest page
atsubmit submit [question name] # submit for latest page
atsubmit show # show questions
atsubmit show [question name] # show latest page (?)
atsubmit test [question name] # run test case using docker
atsubmit result # show all result about you get
atsubmit result [question name] # show result
atsubmit stop # stop server
```

# JSON

## Request

{
	"rcom":"row command (maybe for this app client)"
	"subcmd":"subcommand for atsubmit. get, show, result, test, submit, or stop."
	"cname":"conntest name (example abc150)"
	"qname":"question name (example abc150_a)"
	"file":"file for test or submit"
	"userdir":"client's working directory"
}

## Response

{
	"resstatus":"response status (maybe follow https)"
	"resmsg":"response message"
	"resresult":"result for test, show or result"
}

# subcommand

...

# TODO

 - use docker engine api
 - create html to pdf (?)
 - get [contest] ... get questions in [contest]
