Inductive Nat = zero Unit | succ Nat | symnat Int;

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

lhs = \x: Nat. \y: Nat. \z: Nat. mult (mult x y) z;
rhs = \x: Nat. \y: Nat. \z: Nat. mult x (mult y z);