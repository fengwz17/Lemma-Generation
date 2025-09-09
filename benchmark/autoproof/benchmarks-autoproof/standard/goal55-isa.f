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


lhs = \i:Nat. \j:Nat. \k:Nat. (- (- (nat_to_int i) (nat_to_int j)) (nat_to_int k));
rhs = \i:Nat. \j:Nat. \k:Nat. (- (nat_to_int i) (+ (nat_to_int j) (nat_to_int k)));