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


fac = 
fix (\f: Nat -> Nat.
\x: Nat.
match x with
  zero _ -> succ (zero unit)
| succ n0 -> mult (succ n0) (f n0)
end);

mapmult =
fix (\f: Nat -> Cnat -> Cnat.
\n: Nat.
\cn: Cnat.
match cn with
  czero a -> czero (mult n a)
| csucc {n0, a0} -> csucc {mult n n0, f n a0}
end);

qfac_gen = 
fix (\f: Nat -> Nat -> Cnat.
\x: Nat.
\y: Nat.
match y with
  zero _ -> czero x
| succ y0 -> csucc {x, mapmult (succ y0) (f x y0)}
end);

qfac_exec = 
fix (\f: Cnat -> Nat.
\cn: Cnat.
match cn with
  czero a -> a
| csucc {a0, n0} -> f n0
end);


lhs = \x: Nat. fac x;
rhs = \x: Nat. qfac_exec (qfac_gen (succ (zero unit)) x);