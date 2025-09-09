Inductive Nat = zero Unit | succ Nat | symnat Int;

plus = fix (lambda f: Nat -> Nat -> Nat.
lambda x: Nat.
lambda y: Nat.
match y with
  zero _ -> x
| succ y0 -> succ (f x y0)
end);

minus = fix (lambda f: Nat -> Nat -> Nat.
lambda x: Nat.
lambda y: Nat.
match {x, y} with
  {zero _, zero _} -> zero unit
| {zero _, succ y0} -> zero unit
| {succ x0, zero _} -> succ x0
| {succ x0, succ y0} -> f x0 y0
end
);


lhs = \n:Nat. \m:Nat. (minus n (plus n m));
rhs = zero unit;