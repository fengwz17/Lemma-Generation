Inductive A = x Unit | y Unit | syma Int;
Inductive R = nil3 Unit | eps Unit | atom A | plus {R, R} | seq {R,R} | star R | symr Int;
Inductive Lista = nila Unit | consa {A, Lista} | symlista Int;

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

lhs = \p:R. \s:Lista. recognise (plus {p, p}) s;
rhs = \p:R. \s:Lista. recognise p s;