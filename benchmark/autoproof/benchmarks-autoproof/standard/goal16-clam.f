Inductive Nat = zero Unit | succ Nat | symnat Int;

plus = fix (lambda f: Nat -> Nat -> Nat.
lambda x: Nat.
lambda y: Nat.
match y with
  zero _ -> x
| succ y0 -> succ (f x y0)
end);

even = fix (lambda f: Nat -> Int.
lambda x: Nat.
match x with
  zero _ -> 1
| succ (zero _) -> 0
| succ (succ y0) -> f y0
end);

lhs = \x: Nat. even (plus x x);
rhs = \x: Nat. 1;