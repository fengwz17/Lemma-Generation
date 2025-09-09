Inductive Iint = cint Int | symiint Int;
Inductive Nat = zero Unit | succ Nat | symnat Int;

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

formulapow = 
fix (
lambda f:Nat->Nat->Int.
lambda x:Nat.
lambda y:Nat.
    match y with 
        zero _ -> 1
      | succ z -> * (nat_to_int x) (f x z)
    end
);

lhs = \x:Nat. formulapow (succ (zero unit)) x;
rhs = 1;
