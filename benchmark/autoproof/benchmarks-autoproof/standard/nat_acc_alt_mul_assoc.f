Inductive Nat = zero Unit | succ Nat | symnat Int;

accplus = 
fix (
lambda f:Nat->Nat->Nat.
lambda x:Nat.
lambda y:Nat.
    match x with
        zero _ -> y
      | succ z -> (f z (succ y))
    end
);

accaltmul = 
fix (
lambda f:Nat->Nat->Nat.
lambda x:Nat.
lambda y:Nat.
    match x with 
        zero _ -> zero unit
      | succ z ->
            match y with
                zero _ -> zero unit
              | succ x2 -> (accplus x (accplus x2 (f z x2)))
            end
    end
);

lhs = \x:Nat. \y:Nat. \z:Nat. (accaltmul x (accaltmul y z));
rhs = \x:Nat. \y:Nat. \z:Nat. (accaltmul (accaltmul x y) z);