# lambda.hs

Lambda calculus calculator in haskell.

Read <https://baike.baidu.com/item/%CE%BB%E6%BC%94%E7%AE%97> or <https://zh.wikipedia.org/%CE%BB%E6%BC%94%E7%AE%97>
to know more about **lambda calculus**.

## Example

``` plain
λ> :help
... (help message)
λ> a = 1
λ> a
1
λ> f = λx.x
λ> f a
1
λ> g = λab.a b
λ> g f 2
2 
λ> (λ`a long name`.`a long name`) `a long variable`
`a long variable`
λ> :list
a = 1
f = f = λx.x
g = λab.a b

λ> :quit
```
