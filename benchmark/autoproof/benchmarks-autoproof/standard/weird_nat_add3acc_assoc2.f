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



lhs = \x1:Nat. \x2:Nat. \x3:Nat. \x4:Nat. \x5:Nat. (add3acc (add3acc x1 x2 x3) x4 x5);
rhs = \x1:Nat. \x2:Nat. \x3:Nat. \x4:Nat. \x5:Nat. (add3acc x1 (add3acc x2 x3 x4) x5);