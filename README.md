# atsubmit

AtCoderのサンプル取得,サンプル実行,比較,提出,結果の閲覧を自動で行うHaskell製ツールです.
このツールAtCoderのサーバとコード提出などのやり取りをするためのサーバと
そのサーバとやり取りするためのクライアントで構成されています.

# インストール

```bash
$ make install
```

## サーバの起動

```bash
$ atsubmit-server
```

`atsubmit-server`がdaemonとして起動します.

## ログイン

```bash
$ atsubmit-client login
```
ユーザ名とパスワードを聞かれます.
入力するとログイン処理をしてcookieを保存します.(cookieを$HOME/.atsubmit/cookie に保存します.)

## 問題の取得

### 実行コマンドと概要

```bash
$ atsubmit-client qget abc000_n abc000_m ...
```
問題abc000\_n,問題abc000\_m ... の情報を取得して,生のhtmlをそれぞれ`abc000_n.html`,`abc000_m.html`をコマンド実行したディレクトリに保存します.
コマンドを実行したディレクトリに`abc100_n.html`があればこのファイルから問題の情報を取得します.

```bash
$ atsubmit-client cget abc000 abc001 ...
```

コンテストabc000,コンテストabc001 ... に所属する問題の情報を全て取得して生のhtmlをコマンド実行したディレクトリに保存します.
コマンドを実行したディレクトリに`abc100_n.html`があればその問題のみ,このファイルから問題の情報を取得します.

### 問題の情報

atsubmitは次の情報を持ちます.

+ 問題の元のURL
+ 問題文
+ 制約
+ 入力
+ 入出力例

## 問題の情報の閲覧

```bash
$ atsubmit-client show abc000_n
```

atsubmitが保存した問題`abc000_n`の情報を出力します.

## テストケースの実行

```bash
$ atsubmit-client test abc000_n submit.hs
```

`submit.hs`を問題abc000\_nの入出力例を使ってテストします.

以下の判定に対応しています

+ AC
+ WA
+ TLE

## テストケースを使わない実行

```bash
$ atsubmit-client debug source.hs input.txt
```

`source.hs`に入力`input.txt`を与えた結果を出力します.

## 取得した問題の確認

```bash
$ atsumit-client print
```

atsubmitが取得した問題一覧を表示します.

## AtCoderへの提出

```bash
$ atsubmit-client submit abc000_n submit.hs
```

`submit.hs`を問題abc000\_nの解答としてAtCoderに提出します.

## AtCoder側の結果の確認

```bash
$ atsubmit-client result abc000
```

問題abc000の全ての結果を表示します.

## ログ

```bash
$ atsubmit-client log
```

atsubmit-serverの起動している間に受け取ったコマンドとそのコマンドのステータスを表示します.

## ログアウト

```bash
$ atsubmit-client logout
```

ログアウトの処理をしてcookieを削除します.

## atsubmitの停止

```bash
$ atsubmit-client stop
```

atsubmit-serverを停止します.

# vimプラグイン

エディタのプラグインの一例としてvimのプラグインがあります.

## インストール

```bash
$ make forvim
```

`~/.vimrc`に以下の一行を追加してください.


## 各コマンドに対するvimのコマンド

+ `atsubmit-server` -> `:AtStart`
+ `atsubmit-client login` -> `:AtLogin`
+ `atsubmit-client qget abc000_n` -> `:AtQGet abc000_n`
+ `atsubmit-client cget abc000` -> `:AtCGet abc000`
+ `atsubmit-client show abc000_n` -> `:AtShow abc000_n`
+ `atsubmit-client print` -> `:AtPrint`
+ `atsubmit-client test abc000_n` -> `:AtTest abc000_n`
+ `atsubmit-client debug abc000_n` -> `:AtDebug abc000_n`
+ `atsubmit-client submit abc000_n submit.hs` -> `:AtSubmit abc000_n`
+ `atsubmit-client result abc000` -> `:AtResult abc000`
+ `atsubmit-client log` -> `:AtLog`
+ `atsubmit-client logout` -> `:AtLogout`
+ `atsubmit-client stop` -> `:AtStop`

`atsubmit-client stop`は,vimを閉じた時にも実行されます.
