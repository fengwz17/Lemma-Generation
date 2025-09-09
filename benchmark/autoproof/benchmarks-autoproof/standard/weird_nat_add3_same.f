Inductive Nat = zero Unit | succ Nat | symnat Int;
Inductive List = nil Unit | cons {Nat, List} | symlist Int;
Inductive Llist = nill Unit | consl {List, Llist} | symllist Int;

plus = fix (lambda f: Nat -> Nat -> Nat.
lambda x: Nat.
lambda y: Nat.
match x with
  zero _ -> y
| succ x0 -> succ (f x0 y)
end);

add3 = 
fix (
lambda f:Nat->Nat->Nat->Nat.
lambda x:Nat.
lambda y:Nat.
lambda z:Nat.
    match x with
        zero _ -> 
            match y with
                zero _ -> z
              | succ x2 -> plus (succ x) (f x x2 z)
            end
      | succ x3 -> plus (succ (zero unit)) (f x3 y z)
    end
);

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

lhs = \x:Nat. \y:Nat. \z:Nat. (add3 x y z);
rhs = \x:Nat. \y:Nat. \z:Nat. (add3acc x y z);