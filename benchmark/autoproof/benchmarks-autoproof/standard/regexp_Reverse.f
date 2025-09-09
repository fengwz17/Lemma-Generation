Inductive A = x Unit | y Unit | syma Int;
Inductive R = nil3 Unit | eps Unit | atom A | plus {R, R} | seq {R,R} | star R | symr Int;
Inductive Lista = nila Unit | consa {A, Lista} | symlista Int;
Inductive Pairla = lapair {Lista, Lista} | sympairla Int;
Inductive Plista = pnila Unit | pconsa {Pairla, Plista} | symplista Int;
Inductive Blist = nilb Unit | consb {Bool, Blist} | symblist Int;

seqq = 
(
lambda x:R.
lambda y:R.
    match x with
        _ -> 
            match y with
                _ ->
                    match x with
                        _ -> 
                            match y with
                                _ -> (seq {x, y})
                              | eps _ -> x
                            end
                      | eps _ -> y
                    end
              | nil3 _ -> y
            end
      | nil3 _ -> x
    end
);

pplus = 
(
lambda x:R.
lambda y:R.
    match x with
        nil3 _ -> y
      | _ -> 
        match y with
            nil3 _ -> x
          | _ -> plus {x, y}
        end
    end
);

eqa = 
(
lambda x:A.
lambda y:A.
    match x with
        x _ -> 
            match y with
                x _ -> true 
              | y _ -> false 
            end
      | y _ ->
            match y with
                x _ -> false
              | y _ -> true
            end
    end
);


epsb = 
fix (
lambda f:R->Bool.
lambda x:R.
    match x with
        eps _ -> true
      | plus {p, q} -> (or (f p) (f q))
      | seq {r2, q2} -> (and (f r2) (f q2))
      | star y -> true
      | _ -> false
    end
);

epsr = 
(
lambda x:R.
    if (epsb x) then (eps unit) else (nil3 unit)
);

step = 
fix (
lambda f:R->A->R.
lambda x:R.
lambda y:A.
    match x with
        _ -> nil3 unit
      | atom b -> if (eqa b y) then (eps unit) else (nil3 unit)
      | plus {p, q} -> (pplus (f p y) (f q y))
      | seq {r2, q2} -> (pplus (seqq (f r2 y) q2) (seqq (epsr r2) (f q2 y)))
      | star p2 -> (seqq (f p2 y) x)
    end
);

recognise = 
fix (
lambda f:R->Lista->Bool.
lambda x:R.
lambda y:Lista.
    match y with
        nila _ -> (epsb x)
      | consa {z, xs} -> (f (step x z) xs)
    end
);

deeps = 
fix (
lambda f:R->R.
lambda x:R.
    match x with
        nil3 _ -> nil3 unit
      | eps _ -> nil3 unit
      | atom b -> x
      | plus {p, q} -> plus {(f p), (f q)}
      | seq {r2, q2} -> if (and (epsb r2) (epsb q2)) then plus {(f r2), (f q2)} else x
      | star p2 -> (f p2)
    end
);

eqList = 
fix (
lambda f:Lista->Lista->Bool.
lambda x:Lista.
lambda y:Lista.
    match x with
        nila _ -> 
            match y with
                nila _ -> true
              | consa {z, x2} -> false
            end
      | consa {x3, xs} ->
            match y with
                nila _ -> false
              | consa {y2, ys} -> (and (eqa x3 y2) (f xs ys))
            end
    end
);

split = 
fix (
lambda f:A->Plista->Plista.
lambda x:A.
lambda y:Plista.
    match y with
        pnila _ -> pnila unit
      | pconsa {z, x2} ->
            match z with
                lapair {xs, ys} -> pconsa {(lapair {consa {x, xs}, ys}), (f x x2)}
            end
    end
);

split2 = 
fix (
lambda f:Lista->Plista.
lambda x:Lista.
    match x with
        nila _ -> 
            pconsa {lapair {x,x}, (pnila unit)}
      | consa {y, s} -> pconsa {(lapair {(nila unit), (consa {y,s})}), (split y (f s))}
    end
);

formula = 
fix (
lambda f:R->R->Plista->Blist.
lambda p:R.
lambda q:R.
lambda x:Plista.
    match x with
        pnila _ -> nilb unit
      | pconsa {y, z} -> 
            match y with
                lapair {s1, s2} -> consb {(and (recognise p s1) (recognise q s2)), (f p q z)}
            end
    end
);

or2 = 
fix (
lambda f:Blist->Bool.
lambda x:Blist.
    match x with
        nilb _ -> false
      | consb {y, xs} -> (or y (f xs))
    end
); 

appenda = 
fix (
lambda f:Lista->Lista->Lista.
lambda x:Lista.
lambda y:Lista. 
    match x with
        nila _ -> y
      | consa {h, t} -> consa {h, (f t y)}
    end
);

reverse =
fix (
lambda f:Lista->Lista.
lambda y:Lista.
    match y with
        nila _ -> nila unit
      | consa {z, xs} -> appenda (f xs) (consa {z, (nila unit)})
    end
);

rev = 
fix (
lambda f:R->R.
lambda y:R.
    match y with
        _ -> y
      | plus {b, c} -> plus {(f b), (f c)}
      | seq {c2, b2} -> seq {(f b2), (f c2)}
      | star a2 -> star (f a2)
    end
);


lhs = \p:R. \s:Lista. recognise (rev p) s;
rhs = \p:R. \s:Lista. recognise p (reverse s);