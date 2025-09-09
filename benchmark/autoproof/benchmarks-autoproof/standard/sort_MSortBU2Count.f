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

risers = 
fix (
lambda f:List->Llist.
lambda x:List.
    match x with
        nil _ -> nill unit
      | cons {y, z} -> 
            match z with
                nil _ -> (consl {x, (nill unit)})
              | cons {y2, xs} -> 
                    if (<= (_to_int y) (_to_int y2)) 
                    then 
                        match (f z) with 
                            nill _ -> (nill unit)
                          | consl {ys, yss} -> (consl {(cons {y, ys}), yss})
                         end
                    else
                        consl {(cons {y, (nil unit)}), (f z)}
            end
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

pairwise = 
fix (
lambda f:Llist->Llist.
lambda x:Llist.
    match x with
        nill _ -> x
      | consl {xs, y} -> 
            match y with
                nill _ -> x
              | consl {ys, xss} -> consl {(lmerge xs ys), (f xss)}
            end
    end
);

mergingbu2 = 
fix (
lambda f:Llist->List.
lambda x:Llist.
    match x with
        nill _ -> nil unit
      | consl {xs, y} -> 
            match y with
                nill _ -> xs
              | consl {z, x2} -> (f (pairwise x))
            end
    end
);


msortbu2 = 
(
lambda x:List.
    mergingbu2 (risers x)
);

lhs = \x:Iint. \xs:List. (count (msortbu2 xs) x);
rhs = \x:Iint. \xs:List. (count xs x);
