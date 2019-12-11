# 型と型定義

PureScript でやります.

Haskellでない言い訳をさせてもらうと, PureScript は Functor/Applicative/Monad とかの定義が綺麗で読みやすいんですよ.

本当です. 他意はありません.

- 参照:
  - [PureScript Language Reference](https://github.com/purescript/documentation/blob/master/language/README.md)
  - [Pursuit](https://pursuit.purescript.org/)


## 代数的データ型

参照: [Types#Tagged Unions](https://github.com/purescript/documentation/blob/master/language/Types.md#tagged-unions)

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

値の種類 (Sort).

同じ種類の値を集めた集合ともみられる.

プログラミングの世界ではその定義を構成的に与える (そうでないと扱えないため).

e.g.

```purescript
data Natural
  = Zero
  | Succ Natural

-- a + 0 = a
-- a + Succ(b) = Succ(a + b)
add :: Natural -> Natural -> Natural
add a Zero = a
add a (Succ b) = Succ (add a b)
```


## 型引数をとる型

ある (複数の) 関数によって構成される型, とみられる.

(本当は関数ではなく自己関手, っぽい (始代数とかで調べてみて))

それらの型を構成する関数 (型の値を生成する関数) を, **値コンストラクト** とよぶ.

また, 型引数をとって型を生成する `Maybe` などのことを, **型コンストラクタ** とよぶ.

e.g.

```purescript
-- 型 a に dustbin のような構造を入れる
data Maybe a = Nothing | Just a
-- Nothing :: forall a. Maybe a
-- Just :: forall a. a -> Maybe a

-- これはあくまで例 (PureScript の `pred` は以下とは異なる)
-- dustbin を利用して, 本質的に部分関数なものを関数として定義できる
pred :: Natural -> Maybe Natural
pred Zero = Nothing
pred (Succ n) = Just n
```


## 型クラス

型にある新しい構造を入れるための機能としてみられる.

\`ある操作\`が行える (その型に特殊化された\`ある関数\`が存在する), という事実によってその性質を保証する.

e.g.

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

### なにがうれしいのか

型クラスを実装すると, 型クラスの型を使って実装された関数がすべて使えるようになる

参照: [Data.Eq - purescript-prelude - Pursuit](https://pursuit.purescript.org/packages/purescript-prelude/4.1.1/docs/Data.Eq#t:Eq)

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
test :: Bool
test = Zero /= Succ Zero
```
