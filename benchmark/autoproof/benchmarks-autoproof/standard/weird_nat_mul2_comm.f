Inductive Nat = zero Unit | succ Nat | symnat Int;

plus = fix (lambda f: Nat -> Nat -> Nat.
lambda x: Nat.
lambda y: Nat.
match x with
  zero _ -> y
| succ x0 -> succ (f x0 y)
end);

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


mul2 = 
fix (
lambda f:Nat->Nat->Nat.
lambda x:Nat.
lambda y:Nat.
    match x with
        zero _ -> x
      | succ z ->
            match y with
                zero _ -> y
              | succ x2 -> plus (succ (zero unit)) (add3acc z x2 (f z x2))
            end
    end
);


lhs = \x:Nat. \y:Nat. mul2 x y;
rhs = \x:Nat. \y:Nat. mul2 y x;