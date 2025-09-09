Inductive Nat = zero Unit | succ Nat | symnat Int;

plus = fix (lambda f: Nat -> Nat -> Nat.
lambda x: Nat.
lambda y: Nat.
match y with
  zero _ -> x
| succ y0 -> succ (f x y0)
end);

lhs = \x: Nat. plus (succ x) x;
rhs = \x: Nat. succ (plus x x);