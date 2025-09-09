Inductive Nat = zero Unit | succ Nat | symnat Int;

plus = 
fix (\f: Nat -> Nat -> Nat.
\n: Nat.
\m: Nat.
match m with
  zero _ -> n
| succ m0 -> succ (f n m0)
end);


lhs = \x: Nat. \y: Nat. \z: Nat. plus (plus x y) z;
rhs = \x: Nat. \y: Nat. \z: Nat. plus x (plus y z);