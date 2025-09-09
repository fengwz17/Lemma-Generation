Inductive Nat = zero Unit | succ Nat | symnat Int;
Inductive Iint = cint Int | symiint Int;
Inductive List = nil Unit | cons {Iint, List} | symlist Int;

x = 
fix (lambda f: List -> List -> List. lambda l: List. lambda m: List. 
match l with
  cons {h, t} -> cons {h, (f t m)}
| nil _ -> m
end
);



plus = fix (lambda f: Nat -> Nat -> Nat.
lambda x: Nat.
lambda y: Nat.
match x with
  zero _ -> y
| succ x0 -> succ (f x0 y)
end);

minus = fix (lambda f: Nat -> Nat -> Nat.
lambda x: Nat.
lambda y: Nat.
match x with
  zero _ -> zero unit
| succ x0 -> 
    match y with
          zero _ -> (succ x0)
        | succ y0 -> (f x0 y0)
    end
end
);

drop =
fix (lambda f: Nat -> List -> List. lambda n:Nat. lambda l:List.
match n with
    zero _ -> l
  | succ n0 -> 
    match l with
        cons {h, t} -> (f n0 t)
    |   nil _ -> nil unit
    end
end
);


take =
fix (lambda f: Nat -> List -> List. lambda n:Nat. lambda l:List.
match n with
    zero _ -> nil unit
  | succ n0 -> 
    match l with
        cons {h, t} -> cons {h, (f n0 t)}
    |   nil _ -> nil unit
    end
end
);

rotate = 
fix (
lambda f:Nat->List->List.
lambda y:Nat.
lambda z:List.
    match y with
        zero _ -> z
      | succ z2 -> 
            match z with
                nil _ -> z
              | cons {z22, xs1} -> (f z2 (x xs1 (cons {z22, nil unit})))
            end
    end
);

lt = 
fix (
lambda f:Nat->Nat->Bool.
lambda x:Nat.
lambda y:Nat.
    match y with
        zero _ -> false
      | succ z -> 
            match x with
                zero _ -> true
              | succ n -> (f n z)
            end
    end
);

mod2 = 
fix (
lambda f:Nat->Nat->Nat.
lambda x:Nat.
lambda y:Nat.
    match y with
        zero _ -> zero unit 
      | succ z -> if (lt x y) then x else (f (minus x y) y)
    end
);

length = 
fix (lambda f: List -> Nat.
lambda l: List.
match l with
  cons {h, t} -> succ (f t)
| nil _ -> zero unit
end
);

go = 
fix (
lambda f:Nat->Nat->Nat->Nat.
lambda x:Nat.
lambda y:Nat.
lambda z:Nat.
    match z with
        zero _ -> zero unit
      | succ x2 ->
            match x with
                zero _ -> 
                    match y with
                        zero _ -> zero unit
                      | succ x3 -> minus z y
                    end
              | succ x4 ->
                    match y with 
                        zero _ -> (f x4 x2 z)
                      | succ x5 -> (f x4 x5 z)
                    end
            end
    end 
);

modstructural = 
(
lambda y:Nat.
lambda z:Nat.
    go y (zero unit) z
);

lhs = \n:Nat. \xs:List. rotate n xs;
rhs = \n:Nat. \xs:List. x (drop (modstructural n (length xs)) xs) (take (modstructural n (length xs)) xs);

