Inductive Nat = zero Unit | succ Nat | symnat Int;
Inductive Cnat = czero Nat | csucc {Nat, Cnat} | symcnat Int;

plus = 
fix (\f: Nat -> Nat -> Nat.
\n: Nat.
\m: Nat.
match m with
  zero _ -> n
| succ m0 -> succ (f n m0)
end);

mult = 
fix (\f: Nat -> Nat -> Nat.
\n: Nat.
\m: Nat.
match m with
  zero _ -> zero unit
| succ m0 -> plus n (f n m0)
end);


exp = 
fix (\f: Nat -> Nat -> Nat.
\x: Nat.
\y: Nat.
match y with
  zero _ -> succ (zero unit)
| succ y0 -> mult x (f x y0)
end);

mapmult =
fix (\f: Nat -> Cnat -> Cnat.
\n: Nat.
\cn: Cnat.
match cn with
  czero a -> czero (mult n a)
| csucc {n0, a0} -> csucc {mult n n0, f n a0}
end);

qexp_gen = 
fix (\f: Nat -> Nat -> Nat -> Cnat.
\c: Nat.
\x: Nat.
\y: Nat.
match y with
  zero _ -> czero x
| succ y0 -> csucc {c, mapmult x (f c x y0)}
end);

qexp_exec = 
fix (\f: Cnat -> Nat.
\cn: Cnat.
match cn with
  czero a -> a
| csucc {a0, n0} -> f n0
end);

lhs = \x: Nat. \y: Nat. \z: Nat. mult z (exp x y);
rhs = \x: Nat. \y: Nat. \z: Nat. qexp_exec (qexp_gen z x y);