Inductive Nat = zero Unit | succ Nat | symnat Int;
Inductive Iint = cint Int | symiint Int;

nat_to_int = fix (lambda f: Nat -> Int.
lambda x: Nat.
match x with
  zero _ -> 0
| succ y -> + 1 (f y)
end);

lhs = \m:Nat. \n:Nat. - (+ (nat_to_int m) (nat_to_int n)) (nat_to_int n);
rhs = \m:Nat. nat_to_int m;