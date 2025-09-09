Inductive Nat = zero Unit | succ Nat | symnat Int;

plus = fix (lambda f: Nat -> Nat -> Nat.
lambda x: Nat.
lambda y: Nat.
match x with
  zero _ -> y
| succ x0 -> succ (f x0 y)
end);

altmul = 
fix (
lambda f:Nat->Nat->Nat.
lambda x:Nat.
lambda y:Nat.
    match x with
        zero _ -> zero unit
      | succ z ->
            match y with
                zero _ -> zero unit 
              | succ x2 -> plus (plus (plus (succ (zero unit)) (f z x2)) z) x2
            end
    end
);

lhs = \x:Nat. \y:Nat. \z:Nat. altmul x (altmul y z);
rhs = \x:Nat. \y:Nat. \z:Nat. altmul (altmul x y) z;