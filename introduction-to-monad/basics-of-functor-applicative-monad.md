# Functor, Applicative, Monad の基礎

形式的な説明ではないので, 用語がとても雑です.

ゆるして.


## 直感的な説明

Functor, Applicative, Monad はすべて, 通常の型ではなく, \`型引数をとる型コンストラクタ\` についての性質を表現している.

つまり, 通常の型についての性質を, \`型引数をとる型コンストラクタ\` について一般化しようとしている.


## Monad で副作用を扱える理由

Functor, Applicative をさかのぼって Monad の定義を見ると, Monad はある値を Monad にくるむ操作は提供していても,
くるまれた値を Monad から取り出す操作は一般には提供されていない.

`List` や `Maybe` などは値を取り出す操作があるが, その操作は `List` や `Maybe` に特殊化されており, Monad 一般の操作ではない.

この, Monad から値を取り出す方法が必ずしも存在しない, という性質を利用した Monad として, たとえば `Effect` がある.
`Effect` は **副作用を利用する処理** (副作用そのものではない) を表す.

`random :: Effect Number` は乱数を返す処理を表す Monad で, ここから値を取り出す,
つまり乱数を返す処理を実行すると副作用を取り出すことになってしまう.

しかし, 副作用を取り出すまでは, `random` 自体は副作用にはならない.

そして, `Effect` にはくるんだ値を取り出す方法が定義されていない.

Haskell や PureScript などの純粋関数型の言語では, このように副作用の代わりに Monad を用いて同等な機能を提供する.


## Functor

射像 (変換) の一般化.

ある構造がどのような性質を満たせば, その構造をもつ型の間で変換が行えるか.

```purescript
class Functor f where
  map :: forall a b. (a -> b) -> f a -> f b
  -- Identity: map identity = identity
  -- Composition: map (f <<< g) = map f <<< map g

infixl 4 map as <$>
```

### なにができるのか

- 通常の型での変換 `a -> b` を, Functor 上での変換 `f a -> f b` に持ち上げる

`map` の型を見ると, `(a -> b) -> f a -> f b` となっている.

これは, 関数 `a -> b` を受けとったあと, `f a` を受けとり `f b` に変換する, とみることができる.

もう少しいうと, `map` は通常の型の変換 `a -> b` を Functor `f` の変換 `f a -> f b` に \`持ち上げる\` (\`lift\`), とみることができる


## Applicative

関数適用の一般化.

ある構造がどのような性質を満たせば, その構造をもつ型の間で適用が行えるか.

PureScript ではより一般化して, `apply` を Applicative から切り離し, `Apply` としている.

```purescript
class Apply (Functor f) <= Apply f where
  apply :: forall a b. f (a -> b) -> f a -> f b
  -- Associative compositions: (<<<) <$> f <*> g <*> h = f <*> (g <*> h)

infixl 4 apply as <*>

class Apply f <= Applicative f where
  pure :: forall a. a -> f a
  -- Identity: (pure identity) <*> v = v
  -- Composition: pure (<<<) <*> f g <*> h = f <*> (g <*> h)
  -- Homomorphism: (pure f) <*> (pure x) = pure (f x)
  -- Interchange: u <*> (pure y) = (pure (_ $ y)) <*> u
```

### なにができるのか

- 関数の持ち上げ (Applicative は Functor でもあるので) `map :: (a -> b) -> f a -> f b`
- 任意の型を Applicative でくるむ `pure :: a -> f a`
- Applicative 上での関数適用 `apply :: f (a -> b) -> f a -> f b`

`map` と `apply` を組み合わせたイディオム (として覚えておくと便利な記法) がある.

- 通常の関数 `a -> b -> c` に `f a` と `f b` を適用して `f c` を得る

e.g.

```purescript
data Tuple = Tuple a b

product :: List a -> List b -> List (Tuple a b)
product xs ys = Tuple <$> xs <*> ys

-- <$> = map :: forall f a b. Functor f => (a -> b) -> f a -> f b
-- <*> = apply :: forall f a b. Applicative f => f (a -> b) -> f a -> f b
-- Tuple :: forall a b. a -> b -> Tuple a b
-- xs :: List a
-- ys :: List b
--
-- Tuple :: (a -> (b -> Tuple a b))
-- Tuple <$> xs :: List (b -> Tuple a b)
-- Tuple <$> xs <*> ys :: List (Tuple a b)

-- 引数が増える
type Month = String
type Date = Int
type Year = Int

months :: List Month
months =
  "January" : "February" : "March" : "April"
  : "May" : "June" : "July" : "August" : "September"
  : "October" : "November" : "December" : Nil

dates :: List Date
dates = Data.List.range 1 31

years :: List Year
years = Data.List.range 2018 2019

toString :: Int -> String
toString = Data.Int.toStringAs Data.Int.decimal

timeString :: Month -> Date -> Year -> String
timeString m d y = m <> " " <> toString d <> ", " toString y

timeStrings :: Month -> Date -> Year -> String
timeStrings = timeString <$> months <*> dates <*> years

-- [ "January 1, 2018", "January 1, 2019", "January 2, 2018", "January 2, 2019", ... ]
```


### Monad

手続きの結合の一般化

Monad は今までと少し雰囲気が代わり, `m (m a)` を `m a` に畳み込む操作を提供する.

PureScript ではより一般化して, `bind` を Monad から切り離し, `Bind` としている.

```purescript
class Apply m <= Bind m where
  bind :: forall a b. m a -> (a -> m b) -> m b
  -- Associativity: (x >>= f) >>= g = x >>= (\k -> f k >>= g)

infixl 1 bind as >>=

class (Applicative m, Bind m) <= Monad m
  -- Left identity: pure x >>= f = f x
  -- Right identity: x >>= pure = x
  -- Applicative Superclass: apply = ap

ap :: forall m a b. Monad m => m (a -> b) -> m a -> m b
ap f a = do
  f' <- f
  a' <- a
  pure (f' a')
```

`m a` を `a -> m b` に単純に適用すると `m (m b)` となるが, `bind` はこれを畳み込んで `m b` につぶしてくれる
(`flatten` ともみることができる).

### なにができるのか

- 関数の持ち上げ (Monad は Functor でもあるので) `map :: (a -> b) -> m a -> m b`
- 任意の型を Monad でくるむ (Monad は Applicative でもあるので) `pure :: a -> m a`
- Monad 上での関数適用 (Monad は Applicative でもあるので) `apply :: m (a -> b) -> m a -> m b`
- Monad の入れ子の畳み込み `bind :: m a -> (a -> m b) -> m b`

Monad を用いると副作用相当の表現ができる.

まず, Monad の値 `m a` を1つの処理 (**Monad action**) とみる.
このとき, Monad から値を取り出す操作を, \`その処理の実行\` とみている.

すると, `bind :: m a -> (a -> m b) -> m b` は, Monad action `m a`と, `m a` の結果を受け取って新しい Monad action `m b` を組み立てる関数 `a -> m b` を受けとって, Monad action `m b` を返す関数, とみることができる.

つまり, `bind` を使うと, 前の Monad action の結果を利用するような Monad action を作ることができる.

これは手続き型言語における \`手続き\` 相当のものとみることができる.

また, Monad は 計算 (computation) を表現する手法の1つだともいえる.

do notation をみるとより直感を得やすい.

### Monad としての List

e.g.

```purescript
data Tuple = Tuple a b

product :: List a -> List b -> List (Tuple a b)
product xs ys = do
  x <- xs
  y <- ys
  pure (Tuple x y)

-- bind で書き換える
product xs ys =
  xs >>= (\x ->
    ys >>= (\y ->
      pure (Tuple x y)
    )
  )
```

新しい Monad action を組み立てる中 (`(\y -> ...`) で, 前の Monad action の結果 `x` を利用していることがわかる.

直感的には, do notation の `<-` は `>>=` (`bind`) を逆向きにしたものに相当し, \`Monad action から値を取り出す\` 操作だとみることができる.

### Monad としての Maybe

e.g.

```purescript
data List a = Nil | Cons a (List a)

head :: forall a. List a -> Maybe a
head (x : xs) = Jsut a
head Nil = Nothing

last :: forall a. List a -> Maybe a
last (x : Nil) = Just x
last (_ : xs) = last xs
last Nil = Nothing

getHeadAndLast :: List a -> Maybe (Tuple a a)
getHeadAndLast xs = do
  h <- head xs
  l <- last xs
  pure (Tuple h l)
```

### Monad としての State

`State` は \`状態のある計算\` を表現できる.

- ダイクストラ法など
- キャッシュを用いた計算


## References

- [PureScript Language Reference](https://github.com/purescript/documentation/blob/master/language/README.md)
- [Pursuit](https://pursuit.purescript.org/)
- [続くといいな日記 - Functor / Applicative / Monad が表すもの](https://mizunashi-mana.github.io/blog/posts/2019/04/generalizing-transformation/)
