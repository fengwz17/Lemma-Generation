Inductive Nat = zero Unit | succ Nat | symnat Int;

half = fix (lambda f: Nat -> Nat.
lambda x: Nat.
match x with
  zero _ -> zero unit
| succ (zero _) -> zero unit
| succ (succ y0) -> succ (f y0)
end);

plus = fix (lambda f: Nat -> Nat -> Nat.
\x: Nat.
\y: Nat.
match y with
  zero _ -> x
| succ y0 -> succ (f x y0)
end);

lhs = \x: Nat. \y: Nat. half (plus y x);
rhs = \x: Nat. \y: Nat. half (plus x y);