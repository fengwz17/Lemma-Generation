Inductive Iint = cint Int | symiint Int;
Inductive List = nil Unit | cons {Iint, List} | symlist Int;
Inductive Llist = nill Unit | consl {List, Llist} | symllist Int;
Inductive Nat = zero Unit | succ Nat | symnat Int;

nat_to_int = fix (lambda f: Nat -> Int.
lambda x: Nat.
match x with
  zero _ -> 0
| succ y -> + 1 (f y)
end);

_to_int = (lambda x: Iint.
match x with
  cint a -> a
end
);

eq = (lambda a: Iint.
lambda b: Iint.
if (== (_to_int a) (_to_int b)) then 1 else 0
);

count = 
fix (lambda f: List -> Iint -> Int.
lambda l: List.
lambda x: Iint.
match l with
  cons {h, t} -> + (eq h x) (f t x)
| nil _ -> 0
end
);

take =
fix (lambda f: Int -> List -> List. lambda x:Int. lambda y:List.
    if (<= x 0) then (nil unit) else
        match y with
            nil _ -> y
          | cons {z, xs} -> cons {z, (f (- x 1) xs)}
        end
);

drop =
fix (lambda f: Int -> List -> List. lambda x:Int. lambda y:List.
    if (<= x 0) then y else
        match y with
            nil _ -> y
          | cons {z, xs} -> f (- x 1) xs
        end
);

leq = (lambda x:Iint. lambda y:Iint.
  if (<= (_to_int x) (_to_int y)) then true else false
);

lmerge =
fix (
lambda f:List->List->List.
lambda x:List.
lambda y:List.
    match x with
        nil _ -> y
      | cons {z, x2} -> 
            match y with
                nil _ -> x
              | cons {x3, x4} ->
                    if (leq z x3) then cons {z, (f x2 y)} else cons {x3, (f x x4)}
            end
    end
);


length = 
fix (lambda f: List -> Int.
lambda l: List.
match l with
  cons {h, t} -> + 1 (f t)
| nil _ -> 0
end
);

nmsorttdhalf1 = 
fix (
lambda f:Int->Int.
lambda x:Int.
    if (== x 0) then 0 else
        if (== x 1) then 0 else (+ 1 (f (- x 2)))  
);

nmsorttd =
fix (
lambda f:List->List.
lambda x:List.
    match x with
        nil _ -> x
      | cons {y, z} ->
            match z with
                nil _ -> x
              | cons {x2, x3} ->
                    lmerge (f (take (nmsorttdhalf1 (length x)) x)) (f (drop (nmsorttdhalf1 (length x)) x))
            end
    end
);

lhs = \x:Iint. \xs:List. (count (nmsorttd xs) x);
rhs = \x:Iint. \xs:List. count xs x;

