Inductive Nat = zero Unit | succ Nat | symnat Int;

add3acc = 
fix (
lambda f:Nat->Nat->Nat->Nat.
lambda x:Nat.
lambda y:Nat.
lambda z:Nat.
    match x with
        zero _ -> 
            match y with
                zero _ -> z
              | succ x2 -> (f x x2 (succ z))
            end
      | succ x3 -> (f x3 (succ y) z)
    end
);


lhs = \x:Nat. \y:Nat. \z:Nat. (add3acc x y z);
rhs = \x:Nat. \y:Nat. \z:Nat. (add3acc x z y);