
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

lhs = \m:Nat. \n:Nat. \k:Nat. (- (- (+ (nat_to_int m) 1) (nat_to_int n)) (+ (nat_to_int k) 1));
rhs = \m:Nat. \n:Nat. \k:Nat. (- (- (nat_to_int m) (nat_to_int n)) (nat_to_int k));