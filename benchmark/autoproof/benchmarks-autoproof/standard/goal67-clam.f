Inductive Nat = zero Unit | succ Nat | symnat Int;

even = 
fix (\f: Nat -> Bool.
\n: Nat.
match n with
  zero _ -> true
| succ n0 -> not (f n0)
end);

plus = fix (lambda f: Nat -> Nat -> Nat.
lambda x: Nat.
lambda y: Nat.
match y with
  zero _ -> x
| succ y0 -> succ (f x y0)
end);

lhs = \x: Nat. \y: Nat. succ (succ (plus x y));
rhs = \x: Nat. \y: Nat. (plus (succ (succ x)) y);