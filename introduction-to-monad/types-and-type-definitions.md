# 型と型定義

PureScript でやります.

Haskell ではない言い訳をさせてもらうと, PureScript は Functor / Applicative / Monad などの型クラスの定義が綺麗で読みやすいんです.

本当です. 他意はありません.

- 参照:
  - [PureScript Language Reference](https://github.com/purescript/documentation/blob/master/language/README.md)
  - [Pursuit](https://pursuit.purescript.org/)


## 代数的データ型

- 参照:
  - [Types#Tagged Unions](https://github.com/purescript/documentation/blob/master/language/Types.md#tagged-unions)

**直和** と **直積** によって型を構成できる.

e.g.

```purescript
-- 直和の例
-- これはあくまで例 (PureScript の Boolean はJavaScript の影響もあり少しだけ特殊)
data Boolean = True | False

not :: Boolean -> Boolean
not True = False
not False = True

and :: Boolean -> Boolean
and False _ = False
and _ False = False
and True True = True

-- 直積の例
-- これはあくまで例 (簡単のため Boolean に特殊化している)
data Tuple = Tuple Boolean Boolean

fst :: Tuple Boolean Boolean -> Boolean
fst (Tuple a _) = a

snd :: Tuple Boolean Boolean -> Boolean
snd (Tuple _ b) = b

-- 直和, 直積を併用する例
-- これはあくまで例 (簡単のため Boolean に特殊化している)
data Tree
  = Leaf
  | Node Tree Boolean Tree

depth :: Tree -> Int
depth Leaf = 0
depth (Node left _ right) = max (1 + (depth left)) (1 + (depth right))

isPerfect :: Tree -> Boolean
isPerfect Leaf = true
isPerfect (Node left _ right) = isPerfect left && isPerfect right && depth left == depth right
```


## 型

型とは, 値の種類 (Sort), または同じ種類の値を集めた集合そのもの.

また, 型は \`ある (複数の) 関数によって構成される\` ともみることができる.

これらの型を構成する関数 (型の値を生成する関数) を, **値コンストラクタ** とよぶ.

e.g.

```purescript
-- これはあくまで例 (こんな効率の悪い実装はしない)
data Natural = Zero | Succ Natural

-- Zero は定数関数ともみることができる (もちろん Natural の値の1つとみることもできる)
-- Zero :: Natural
-- Succ :: Natural -> Natural

-- a + 0 = a
-- a + (b + 1) = (a + b) + 1
add :: Natural -> Natural -> Natural
add a Zero = a
add a (Succ b) = Succ (add a b)
```


## 型引数をとる型

ひとつの型に特殊化せず, 一般化された型を用いて型を定義することができる.

また, 型引数をとって型を生成する `Maybe` などのことを, **型コンストラクタ** とよぶ.

e.g.

```purescript
-- ある型 a に dustbin のようなもの (Nothing) を入れる
data Maybe a = Nothing | Just a
-- Nothing :: forall a. Maybe a
-- Just :: forall a. a -> Maybe a

-- これはあくまで例 (PureScript の `pred` は以下とは異なる)
-- dustbin を利用して, 本質的に部分関数なものを関数として定義できる
pred :: Natural -> Maybe Natural
pred (Succ n) = Just n
-- 0 の前者は存在しないので, dustbin に投げる (雑なことば)
pred Zero = Nothing
```


## 種 (kind)

型の型のようなものを, **種** (**kind**) とよぶ.

コンパイラに怒られるときによく見かける.

e.g.

```purescript
data Boolean = True | False

-- Boolean の種は `*`

data List a = Nil | Cons a (List a)

-- List の種は `* -> *`
-- List Int の種は `*`
```


## 型クラス

型または型コンストラクタに, ある構造を入れるための機能としてみることができる.

\`ある操作\`が行える (その型に特殊化された\`ある関数\`が定義されている), という事実によってその構造が満たすべき性質を保証する.

型だけでなく, 型コンストラクタについても性質を記述できることに注意.

e.g.

- 参照:
  - [Data.Semiring - purescript-prelude - Pursuit](https://pursuit.purescript.org/packages/purescript-prelude/4.1.1/docs/Data.Semiring#t:Semiring)

```purescript
-- 型 a に構造 Semiring を入れるためには, 以下の関数が定義されている必要がある
class Semiring a where
  add :: a -> a -> a
  zero :: a
  mul :: a -> a -> a
  one :: a

infixl 6 add as +
infixl 7 mul as *

-- 型クラスでは, それぞれの関数が満たすべき性質までは保証できない
-- なので, それらは自力で保証する必要がある

-- e.g. Semiring (読まなくていいです)
-- Comutative monoid under addition:
  -- Associativity: (a + b) + c = a + (b + c)
  -- Identity: zero + a = = a + zero = zero
  -- Commutative: a + b = b + a
-- Monoid under multiplication:
  -- Associativity: (a * b) * c = a * (b * c)
  -- Identity: one * a = a * one = a
-- Multiplication distributes over addition:
  -- Left distributivity: a * (b + c) = (a * b) + (a * c)
  -- Right distributivity: (a + b) * c = (a * c) + (b * c)
-- Annihilation: zero * a = a * zero = zero


-- 特殊化する
instance semiringNatural :: Semiring Natural where
  add m Zero = Zero
  add m (Succ n) = Succ (add m n)

  zero = Zero

  mull m Zero = Zero
  mull m (Succ n) = add m (mull m n)

  one = Succ Zero
```

e.g. 型コンストラクタに対する型クラス

(型コンストラクタに対する型クラスはどうしても難しくなるので,
今回の本題とは関係が薄いですし読み飛ばしてもいいです)

- 参照:
  - [Data.Semigroup - purescript-prelude - Pursuit](https://pursuit.purescript.org/packages/purescript-prelude/4.1.1/docs/Data.Semigroup#t:Semigroup)
  - [Data.Monoid - purescript-prelude - Pursuit](https://pursuit.purescript.org/packages/purescript-prelude/4.1.1/docs/Data.Monoid#t:Monoid)
  - [Data.Foldable - purescript-foldable-traversable - Pursuit](https://pursuit.purescript.org/packages/purescript-foldable-traversable/4.1.1/docs/Data.Foldable#t:Foldable)

```purescript
class Semigroup a where
  append :: a -> a -> a
  -- Associativity: (x <> y) <> z = x <> (y <> z)

infixl 5 append as <>

class Semigroup m <= Monoid m where
  mempty :: m
  -- Left unit: (mempty <> x) = x
  -- Right unit: (x <> mempty) = x

class Foldable f where
  foldr :: forall a b. (a -> b -> b) -> b -> f a -> b
  foldl :: forall a b. (b -> a -> b) -> b -> f a -> b
  foldMap :: forall a m. Monoid m => (a -> m) -> f a -> b


data List a = Nil | Cons a (List a)

infixr 6 Cons as :

-- List a に対する型クラスの実装
instance semigroupList :: Semigroup (List a) where
  append xs ys = foldr (:) ys xs

-- List a に対する型クラスの実装
instance monoidList :: Monoid (List a) where
  mempty = Nil

-- List (**型コンストラクタ**) に対する型クラスの実装
instance foldableList :: Foldable List where
  foldr f b = foldl (flip f) b <<< rev
    where
    rev = foldl (flip Cons) Nil
  foldl f = go
    where
    go b = case _ of
      Nil -> b
      a : as -> go (f b a) as
  foldMap f = foldl (\acc -> append acc <<< f) mempty
```


### なにがうれしいのか

型クラスを実装すると, 型クラスの型を使って実装された関数がすべて使えるようになる

- 参照:
  - [Data.Eq - purescript-prelude - Pursuit](https://pursuit.purescript.org/packages/purescript-prelude/4.1.1/docs/Data.Eq#t:Eq)

e.g.

```purescript
class (Eq a) <= Ord a where
  eq :: a -> a -> Boolean

infix 4 eq as ==

notEq :: Eq a => a -> a -> a
notEq x y = (x == y) == false

infix 4 eq as /=

-- インスタンスの実装
instance eqNatural :: Eq Natural where
  eq Zero Zero = true
  eq Zero (Succ _) = false
  eq (Succ _) Zero = false
  eq (Succ m) (Succ n) = eq m n

-- Natural で notEq が使える
-- > Zero /= Succ Zero
-- = true
```

e.g.

- 参照:
  - [Data.Semiring - purescript-prelude - Pursuit](https://pursuit.purescript.org/packages/purescript-prelude/4.1.1/docs/Data.Semiring#t:Semiring)
  - [Data.Foldable - purescript-foldable-traversable - Pursuit](https://pursuit.purescript.org/packages/purescript-foldable-traversable/4.1.1/docs/Data.Foldable#t:Foldable)

```purescript
product :: forall a f. Foldable f => Semiring a => f a -> a
product = foldl (*) one

-- (List Natural) で使える
-- > product (Succ Zero : Succ (Succ Zero) : Succ (Succ (Succ Zero)) : Nil)
-- = Succ (Succ (Succ (Succ (Succ (Succ Zero)))))
```
