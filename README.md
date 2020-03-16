# atsubmit

AtCoderのサンプル取得,サンプル実行,比較,提出,結果の閲覧を自動で行うHaskell製ツールです.
このツールAtCoderのサーバとコード提出などのやり取りをするためのサーバと
そのサーバとやり取りするためのクライアントで構成されています.

## サーバの起動

```bash
$ atsubmit
```

起動時にユーザ名とパスワードの入力を要求されるので入力してください.
`atsubmit`がdaemon化します.

## 問題の取得

コンテスト単位は

```bash
$ atsubmit get abc150
```

問題単位は

```bash
$ atsubmit get abc150_a
```

とすればコンテスト単位ならばコンテスト単位で,問題単位であれば問題単位でテストケースの取得と
コマンドを実行したディレクトリに問題のhtmlファイルを生成します.

## テストケースの実行

abc150のA問題に対してテストケースを実行したい場合は
```bash
$ atsubmit test abc150_a submit.hs
```

とすればテストケースが正しいかどうかを判定をします.
この判定にはdockerを用いています.(変更するかも?)

## 取得した問題とそのテストケースの確認

サーバが取得した問題は次のコマンドで確認できます
```bash
$ atsubmit show
```

また,問題のテストケースを確認したい場合は以下のコマンドで確認できます.
```bash
$ atsubmit show abc150_a
```

## AtCoder側の結果の確認

`atsubmit`が取得した問題の自分の結果を確認したい場合は以下のコマンドで確認できます.

```bash
$ atsubmit result
```

## atsubmitの停止

`atsubmit`の停止は以下のコマンドで実行できます.
```bash
$ atsubmit stop
```


```bash
atsubmit # start atsubmit server
atsubmit login # relogin and get cookie [ERROR] canot input (remove?)
atsubmit get [question name] # get latest page
atsubmit get [contest name] # get question infomations in [contest name]
atsubmit submit [question name] # submit for latest page
atsubmit show # show questions
atsubmit show [question name] # show latest page (?)
atsubmit test [question name] # run test case using docker
atsubmit result # show all result about you get
atsubmit result [question name] # show result
atsubmit stop # stop server
```
# JSON

## First 
{
	"socksize":"size of one receiver"
	"datasize":"all size of sender message"
}

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

## plugin sumple

look asvim

# TODO

 - use docker engine api
