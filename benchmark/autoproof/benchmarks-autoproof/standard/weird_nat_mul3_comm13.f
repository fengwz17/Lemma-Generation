Inductive Nat = zero Unit | succ Nat | symnat Int;

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

mul3 = 
fix (
lambda f:Nat->Nat->Nat->Nat.
lambda x:Nat.
lambda y:Nat.
lambda z:Nat.
       match x with
        zero _ -> x
      | succ x2 ->
          match y with
              zero _ -> y
            | succ x3 ->
                match z with
                    zero _ -> z
                  | succ x4 ->
                      let fail =
                        plus
                          (succ (zero unit))
                          (add3
                             (f x2 x3 x4)
                             (add3
                                (f (succ (zero unit)) x3 x4) (f x2 (succ (zero unit)) x4)
                                (f x2 x3 (succ (zero unit))))
                             (add3 x2 x3 x4)) 
                      in match x2 with
                             zero _ ->
                               match x3 with
                                   zero _ ->
                                     match x4 with
                                         zero _ -> z
                                       | succ x5 -> fail
                                     end
                                 | succ x6 -> fail
                               end
                           | succ x7 -> fail
                         end
                end
          end
    end 
);

lhs = \x:Nat. \y:Nat. \z:Nat. (mul3 x y z);
rhs = \x:Nat. \y:Nat. \z:Nat. (mul3 z y x);