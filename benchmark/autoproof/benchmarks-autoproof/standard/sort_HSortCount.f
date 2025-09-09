Inductive Iint = cint Int | symiint Int;
Inductive List = nil Unit | cons {Iint, List} | symlist Int;
Inductive Heap = nilheap Unit | node {Heap, Iint, Heap} | symheap Int;
Inductive Hlist = nilh Unit | consh {Heap, Hlist} | symhlist Int;

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

hmerge = 
fix (
lambda f:Heap->Heap->Heap.
lambda x:Heap.
lambda y:Heap.
    match x with
        nilheap _ -> y
      | node {z, x2, x3} ->
            match y with
                nilheap _ -> x
              | node {x4, x5, x6} -> if (<= (_to_int x2) (_to_int x5)) 
                                     then (node {(f x3 y), x2, z}) 
                                     else (node {(f x x6), x5, x4})
            end
    end
);

tolist = 
fix (
lambda f: Heap->List.
lambda x:Heap.
    match x with
        nilheap _ -> nil unit
      | node {p, y, q} -> cons {y, (f (hmerge p q))}
    end
);

hinsert = 
(
lambda x:Iint.
lambda y:Heap.
    hmerge (node {nilheap unit, x, nilheap unit}) y
);


toheap = 
fix (
lambda f:List->Hlist.
lambda x:List.
    match x with  
        nil _ -> nilh unit
      | cons {y, z} -> consh {node {(nilheap unit), y, (nilheap unit)}, (f z)}
    end
);

hpairwise = 
fix (
lambda f:Hlist->Hlist.
lambda x:Hlist.
    match x with
        nilh _ -> x
      | consh {p, y} -> 
            match y with
                nilh _ -> x
              | consh {q, qs} -> consh {(hmerge p q), (f qs)}
            end
    end
);

hmerging = 
fix (
lambda f:Hlist->Heap.
lambda x:Hlist.
    match x with
        nilh _ -> nilheap unit
      | consh {p, y} ->
            match y with
                nilh _ -> p
              | consh {z, x2} -> f (hpairwise x)
            end
    end
);

toheap2 = 
fix (
lambda f:List->Heap.
lambda x:List.
    hmerging (toheap x)
);

hsort = 
(
lambda x:List.
    tolist (toheap2 x)
);


hsort2 =
(
lambda x:List.
    tolist (toheap2 x)
);

lhs = \x:Iint. \xs:List. count (hsort xs) x;
rhs = \x:Iint. \xs:List. count xs x;