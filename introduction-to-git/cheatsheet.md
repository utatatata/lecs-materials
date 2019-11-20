# Gitチートシート

## ローカル作業の流れ

```
# ステージング
$ git add -A

# コミット
$ git commit

# リモートにプッシュ
$ git push -u origin master
```

### ステージング

```
# すべてステージング
$ git add -A

# ./src 以下のファイルをすべてステージング
$ git add ./src

# ./test 以下の *.hs (ワイルドカード)をすべてステージング
# ディレクトリを省略した場合 (*.hs) は, カレントディレクトリを指定したこと (./*.hs) になる
$ git add ./test/*.hs
```

### コミット

```
# デフォルトのエディタが起動し, コミットメッセージを編集できる
$ git commit

# メッセージを指定できるが基本1行しか書けない (非推奨)
$ git commit -m "a commit message"
```

### プッシュ

```
# `-u` を付けると, この設定がデフォルトになり, 次回以降 `git push` だけで同じ結果になる
# これはブランチ (この例なら master)に対する設定
$ git push -u origin master

# 2回目以降
$ git push
```

## リモート作業の流れ

```
# プル
$ git pull

# コンフリクトしたなら, 該当ファイルを編集してステージング, コミット

# プッシュ
$ git push
```

### フェッチ&マージ

```
# リモートリポジトリの情報を取得
$ git fetch

# 現在のブランチにマージ
$ git merge FETCH_HEAD
```

### プル

```
# リモートリポジトリの情報を取得し, 現在のブランチにマージ
$ git pull
```

### コンフリクト

コンフリクトしたファイルは以下のようにコンフリクト位置が示された状態になる.

- Main.hs

```
module Main where

main :: IO ()
main = do
<<<<<<< HEAD
  # ローカルリポジトリとワーキングディレクトリの差分
  putStrLn "Hello, world!"
=======
  # ローカルリポジトリとリモートリポジトリの差分
  putStrLn "Hello, Haskell!"
>>>>>>> develop
```

編集して, どちらかを残す.
以下の例はローカルリポジトリの編集を残す場合.

- Main.hs

```
module Main where

main :: IO ()
main = do
  putStrLn "Hello, world!"
```


## ステージングを取り消したい

```
# 1ファイルだけ戻す
$ git reset HEAD Lib.hs

# ./src/ 以下をすべて戻す
$ git reset HEAD ./src/
```

- `HEAD` ... 直前のコミット
- `HEAD^` ... `HEAD` の1つ前のコミット
- `HEAD{n}` ... `HEAD` のn個前のコミット (e.g. `HEAD^2`)

コミットのハッシュ値を直接指定することもできる.
コミットのハッシュ値は `git log` で確認できる.

e.g.

```
$ git log
commit 6c35bd3070d94bee36a347253ba568d2392e2f09 (HEAD -> master, origin/master)
Author: utakuma <i@utakuma.info>
Date:   Wed Nov 20 06:39:49 2019 +0900

    first commit

$ git reset 6c35bd3070d94bee36a347253ba568d2392e2f09 ./src/Main.hs
```


## コミットを取り消したい

```
# 1つ前のコミットの状態に戻す
$ git reset --hard HEAD^1

# n個前のコミットの状態に戻す
$ git reset --hard HEAD^n
```


## 前のコミットに戻したい

```
# 指定ハッシュ値のコミットの状態に戻る
# `HEAD` が指定したコミットに変更される
$ git checkout 6c35bd3070d94bee36a347253ba568d2392e2f09

# 1つ前のコミットの状態に戻る
$ git checkout HEAD^1

# `git reset` と異なり, `git checkout HEAD~1` で1つ前に戻った後, 元の `HEAD` に戻ることができる
# 元の `HEAD` のハッシュ値を覚えておく必要がある
$ git checkout 3f72fc952de2264f96da5d04cafad1a390673f26
```

## 前のコミットにチェックアウトしたが, やっぱり元の `HEAD` に戻りたい

元の `HEAD` のハッシュ値を覚えているなら, そこにチェックアウトする.

覚えていなかったときは以下のようにする.

```
$ git log
commit 3f72fc952de2264f96da5d04cafad1a390673f26 (HEAD -> master)
Author: utakuma <i@utakuma.info>
Date:   Wed Nov 20 15:30:17 2019 +0900

    update Main.hs

commit 6c35bd3070d94bee36a347253ba568d2392e2f09 (origin/master)
Author: utakuma <i@utakuma.info>
Date:   Wed Nov 20 06:39:49 2019 +0900

    first commit

# 1つ前のコミットに戻る
$ git checkout HEAD^

# `git log` しても元のコミットが見えない (`HEAD` の位置が変わるため)
$ git log
commit 6c35bd3070d94bee36a347253ba568d2392e2f09 (HEAD, origin/master)
Author: utakuma <i@utakuma.info>
Date:   Wed Nov 20 06:39:49 2019 +0900

    first commit

# 実はチェックアウトは一時的なブランチを作って, そこで作業している
# ブランチ一覧を表示する
$ git branch
* (HEAD detached at 6c35bd3) # 一時ブランチ
  master                     # 元いたブランチ

# なので, 元のブランチに戻ればよい
$ git checkout master

$ git branch
*  master

$ git log
commit 3f72fc952de2264f96da5d04cafad1a390673f26 (HEAD -> master)
Author: utakuma <i@utakuma.info>
Date:   Wed Nov 20 15:30:17 2019 +0900

    update Main.hs

commit 6c35bd3070d94bee36a347253ba568d2392e2f09 (origin/master)
Author: utakuma <i@utakuma.info>
Date:   Wed Nov 20 06:39:49 2019 +0900

    first commit
```
