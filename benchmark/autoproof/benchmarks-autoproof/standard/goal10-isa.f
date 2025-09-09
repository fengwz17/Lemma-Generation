Inductive Nat = zero Unit | succ Nat | symnat Int;
Inductive Iint = cint Int | symiint Int;

_to_int = (lambda x: Iint.
match x with
  cint a -> a
end
);

nat_to_int = fix (lambda f: Nat -> Int.
lambda x: Nat.
match x with
  zero _ -> 0
| succ y -> + 1 (f y)
end);

eq = (lambda a: Iint.
lambda b: Iint.
if (== (_to_int a) (_to_int b)) then 1 else 0
);


lhs = \m:Nat. - (nat_to_int m) (nat_to_int m);
rhs = 0;