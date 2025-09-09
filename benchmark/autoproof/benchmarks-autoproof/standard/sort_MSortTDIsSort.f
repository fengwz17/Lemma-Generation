Inductive Iint = cint Int | symiint Int;
Inductive List = nil Unit | cons {Iint, List} | symlist Int;
Inductive Llist = nill Unit | consl {List, Llist} | symllist Int;


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
                    if (<= (_to_int z) (_to_int x3)) then cons {z, (f x2 y)} else cons {x3, (f x x4)}
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

msorttd = 
fix (
lambda f:List->List.
lambda x:List.
    match x with
        nil _ -> x
      | cons {y, z} -> 
            match z with 
                nil _ -> x
              | cons {x2, x3} -> lmerge (f (take (/ (length x) 2) x)) (f (drop (/ (length x) 2) x))
            end
    end
);

less = (lambda a: Iint. lambda b: Iint.
(< (_to_int a) (_to_int b))
);


insert = 
fix (lambda f: Iint -> List -> List. lambda n:Iint. lambda l:List.
    match l with
        nil _ -> cons {n, nil unit}
      | cons {h,t} -> if (less n h) 
                        then cons {n, cons {h, t}} 
                        else (f n t)
    end
);

isort = 
fix (lambda f:List -> List. lambda l:List.
match l with
    nil _ -> nil unit
  | cons {h, t} -> insert h (f t)
end
);


lhs = \xs:List. msorttd xs;
rhs = \xs:List. isort xs;

