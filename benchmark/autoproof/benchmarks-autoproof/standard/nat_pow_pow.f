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

plus = fix (lambda f: Nat -> Nat -> Nat.
lambda x: Nat.
lambda y: Nat.
match x with
  zero _ -> y
| succ x0 -> succ (f x0 y)
end);

times = 
fix (
lambda f: Nat -> Nat -> Nat.
lambda x:Nat.
lambda y:Nat.
    match x with
        zero _ -> x
      | succ z -> plus z (f z y)
    end
);

formulapow2 = 
fix (
lambda f:Nat->Nat->Int.
lambda x:Nat.
lambda y:Nat.
    match y with
        zero _ -> 1
      | succ z -> * (nat_to_int x) (f x z)
    end
);

formulapow3 = 
fix (
lambda f:Nat->Nat->Nat.
lambda x:Nat.
lambda y:Nat.
    match y with
        zero _ -> (succ (zero unit))
      | succ z -> times x (f x z)
    end
);

lhs = \x:Nat. \y:Nat. \z:Nat. formulapow x (times y z);
rhs = \x:Nat. \y:Nat. \z:Nat. formulapow2 (formulapow3 x y) z;
