# Git基本設定

## グローバルとローカル

Gitの設定にはグローバル(常に有効)とグローバル(プロジェクト内でだけ有効)があります.

- `~/.gitconfig` グローバルの設定
- `./.git/config` ローカルの設定

プロジェクト内ではローカルの設定が優先されます.

## 設定の確認

```
# すべて表示 (ローカル)
$ git config -l # or `git config --list`

$ すべて表示 (グローバル) 
$ git config --global -l

$ 指定して表示
$ git config --global user.name
```


## 設定の方法

グローバルの設定ファイルは `~/.gitconfig` ですが, なければ作成してください.

ローカルの設定ファイルは `git init` によって自動的に作成されます.

### 設定ファイルを直接編集して設定

```
$ git config --global user.name
# 何も出力されない
```

`~/.gitconfig` に以下を追加.

```
[user]
  - name = your name
```

```
$ git config --global user.name
your name
```


### CLIコマンドで設定

```
$ git config --global user.name
# 何も出力されない

$ git config --global user.name "your name"

$ git config --global user.name
your name
```


## 基本的な設定 (推奨)

`~/.gitconfig` (グローバル)

```
# ユーザに関する設定
[user]
  # コミットログなどに残る
  email = yours@example.com
  name = your name

[merge]
  # fast-forward マージを禁止
  # ffだと「ブランチを切った」という履歴が残らないため
  ff = false

[pull]
  # pull は fetch と merge FETCH_HEADにわけれられるが, 後者において
  # fast-forward マージを強制
  # ffにしないと, リモートリポジトリの変更を反映しただけなのに「ブランチを切った」という履歴が残ってしまう
  ff = only
```
