Inductive Nat = zero Unit | succ Nat | symnat Int;

op = 
fix (
lambda f:Nat->Nat->Nat->Nat->Nat.
lambda x:Nat.
lambda y:Nat.
lambda z:Nat.
lambda x2:Nat.
    match z with
      zero _ ->
        match x with
          zero _ -> x2
        | succ x3 -> f x3 y y x2
        end
    | succ x4 -> f x y x4 (succ x2)
    end
);

lhs = \a:Nat. \b:Nat. \c:Nat. \d:Nat. \e:Nat. (op (op a b (zero unit) (zero unit)) c d e);
rhs = \a:Nat. \b:Nat. \c:Nat. \d:Nat. \e:Nat. (op a (op b c (zero unit) (zero unit)) d e);