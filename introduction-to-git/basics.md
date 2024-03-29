# Git基礎


## Gitとは

ファイルのバージョン管理を賢く行ってくれるツールです.

ダメな例:

```
├── 目次.txt
├── レジュメ.txt
└── test/
    ├── 作業.txt
    ├── 作業2.txt
    └── 作業_2019-11-11.txt
    ├── 作業_old.md
    ├── 作業_最新.pdf
    ├── 作業copy.md
    └── 作業ログ_2019-11-19.txt
```

Gitを使うことでファイルの変更を履歴として保存し, 簡単に復元することができます.

また, Gitはバージョン管理システムの中でも分散型とよばれる形式を採用しています.

これによって, 複数人で作業をする場合に同じファイルを編集して上書きしてしまうといった事故を防ぐことができます.


## リポジトリ

リポジトリとは, Gitが履歴を保存している場所 (e.g. `project/.git/`) のことです.

また, Gitが履歴を管理するディレクトリ (e.g. `project/`) のことをリポジトリとよぶこともあります.

通常は1つのプロジェクトなどを1つのリポジトリとしてGitで管理します.

### リモートリポジトリとローカルリポジトリ

複数人で作業する場合, 編集ファイルはサーバに置くことが多いと思います.

Gitでは, サーバにあるリポジトリをリモートリポジトリ, ローカル(手元のPCなどの端末)にあるリポジトリをローカルリポジトリとよびます.

Gitは分散型なので, リモートリポジトリを直接は編集しません.
リモートリポジトリをローカルにクローン(コピーのこと)して, ローカルリポジトリとして変更を加えていきます.

以下では, Gitの作業の流れをローカルでの履歴の保存の流れとリモート-ローカル間の同期の流れにわけて説明します.

また, ブランチを使った作業の流れについても説明します.


## 全体の流れ (ローカル)

![全体の流れ (ローカル)](./resources/git-over-view-local.png)

1. ファイルを編集 (ワーキングツリー)
2. ステージング (ステージングエリア)
3. コミット (リポジトリ)


## コミット

コミットはファイルの変更履歴の単位です.
1つのコミットが1つの履歴だと考えてください.

また, コミットを作成することを「コミットする」などと言います.

コミットにはどのファイルがどれだけ変更されたかの差分の情報と, コミットメッセージが含まれます.

ファイルの変更差分は何行削除され, 何行追加されたかで表現されます.
コミットメッセージはコミット時に編集します.

## ステージング (インデックス)

ステージングエリアは一時的にファイルの変更差分を置いておける領域です.

コミット時, ステージングエリアに置かれたすべてのファイルの変更差分がそのコミットに含まれます.

つまり, ステージングエリアはどのファイルの変更差分をコミットに含めるかを選別するためのものです.


## ワーク (ワーキング) ツリー

ワーキングツリーとは, Gitが管理しているディレクトリのことです.

ファイルの変更差分は, ワーキングツリー(つまり現在編集しているファイル)からステージング領域へ置かれ, 最終的にリポジトリにコミットとして保存されます.

## ヘッド

ヘッドは現在編集している履歴を指し示します.

たとえば1つ前の履歴に戻ってファイルを編集し直す場合などでは, リポジトリの内容は変化しませんがヘッドの位置が変化します.


## 全体の流れ (リモート-ローカル間)

![全体の流れ (リモート-ローカル間)](./resources/git-over-view-remote-to-local.png)

1. フェッチ (リモートリポジトリの情報のみ取得)
2. マージ (リモートリポジトリの変更をローカルリポジトリのブランチに反映)
3. コンフリクトした場合, その修正
4. プッシュ (ローカルリポジトリのブランチの情報をリモートリポジトリに反映)

![全体の流れ (リモート-ローカル間 プッシュ)](./resources/git-over-view-remote-to-local-push.png)


## フェッチ

リモートリポジトリの情報を取得し, ローカルリポジトリに無名のブランチとして取り込みます.
`FETCH_HEAD` という名前でマージしたりできます.

リポジトリに反映させるだけなので, ワーキングツリー (つまり編集中のファイル) は変化しません.


## マージ

ブランチに他のブランチの変更を反映させる操作をマージとよびます.

フェッチで取ってきたリモートリポジトリの情報を取得するときもマージを使います.


## プル

`FETCH_HEAD`のフェッチとマージを同時に行います.


## コンフリクト

現在のブランチとマージ先のブランチで同じファイルを別で編集していた場合, ファイルが競合 (コンフリクト) してマージに失敗します.

コンフリクトしたファイルは, コンフリクトした内容がわかりやすく付け加えられています.

ファイルを直接編集するか, コマンドによってコンフリクトを修正できます.

修正後, ファイルをステージングしてコミットすると, 失敗したマージがコミットされます.


## プッシュ

プッシュとは, ローカルリポジトリのブランチをリモートリポジトリに反映させることです.

ローカルリポジトリとリモートリポジトリのブランチがコンフリクトするとプッシュに失敗します.

その場合, プルによってコンフリクトを解消してから再度プッシュします.
