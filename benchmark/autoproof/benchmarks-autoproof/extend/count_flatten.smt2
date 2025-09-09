(set-logic ALL_SUPPORTED)

(declare-datatypes () ((Nat (zero) (succ (succ0 Nat)))))

(declare-datatypes () ((Iint (cint (cint0 Int)))))

(declare-datatypes () ((List (nil) (cons (cons0 Iint) (cons1 List)))))

(declare-datatypes () ((Tree (leaf (leaf0 Iint)) (bin (bin0 Tree) (bin1 Tree)))))












(declare-fun append (List List) List)
(declare-fun len (List) Int)
(declare-fun _to_int (Iint) Int)
(declare-fun sum (List) Int)
(declare-fun count (List) Int)
(declare-fun flatten (Tree) List)
(declare-fun sumtree (Tree) Int)
(declare-fun lentree (Tree) Int)
(declare-fun counttree (Tree) Int)
(declare-fun rev (List) List)
(declare-fun less (Iint Iint) Bool)
(declare-fun linsert (Iint List) List)
(declare-fun sort (List) List)
(declare-fun insapp (List List) List)
(declare-fun tsort (Tree) List)











(assert (forall ((m List)  (h Iint) (t List)) (= (append m (cons h t)) (cons h (append m t)))))
(assert (forall ((m List)) (= (append m nil) m)))
(assert (forall (  (h Iint) (t List)) (= (len  (cons h t)) (+ 1 (len t)))))
(assert (= (len  nil) 0))
(assert (forall ( (a Int)) (= (_to_int  (cint a)) a)))
(assert (forall (  (h Iint) (t List)) (= (sum  (cons h t)) (+ (_to_int h) (sum t)))))
(assert (= (sum  nil) 0))
(assert (forall (  (h Iint) (t List)) (= (count  (cons h t)) (ite (>= (_to_int h) 0) (+ 1 (count t)) (count t)))))
(assert (= (count  nil) 0))
(assert (forall ( (a Iint)) (= (flatten  (leaf a)) (cons a nil))))
(assert (forall (  (xl Tree) (xr Tree)) (= (flatten  (bin xl xr)) (append (flatten xl) (flatten xr)))))
(assert (forall ( (a Iint)) (= (sumtree  (leaf a)) (_to_int a))))
(assert (forall (  (xl Tree) (xr Tree)) (= (sumtree  (bin xl xr)) (+ (sumtree xl) (sumtree xr)))))
(assert (forall ( (a Iint)) (= (lentree  (leaf a)) 1)))
(assert (forall (  (xl Tree) (xr Tree)) (= (lentree  (bin xl xr)) (+ (lentree xl) (lentree xr)))))
(assert (forall ( (a Iint)) (= (counttree  (leaf a)) (ite (>= (_to_int a) 0) 1 0))))
(assert (forall (  (xl Tree) (xr Tree)) (= (counttree  (bin xl xr)) (+ (counttree xl) (counttree xr)))))
(assert (forall (  (h Iint) (t List)) (= (rev  (cons h t)) (append (cons h nil) (rev t)))))
(assert (= (rev  nil) nil))
(assert (forall ((a Iint) (b Iint)) (= (less a b) (< (_to_int a) (_to_int b)))))
(assert (forall ((n Iint)) (= (linsert n nil) (cons n nil))))
(assert (forall ((n Iint)  (h Iint) (t List)) (= (linsert n (cons h t)) (ite (< (_to_int n) (_to_int h)) (cons n (cons h t)) (cons h (linsert n t))))))
(assert (= (sort  nil) nil))
(assert (forall (  (h Iint) (t List)) (= (sort  (cons h t)) (linsert h (sort t)))))
(assert (forall ((m List)  (h Iint) (t List)) (= (insapp m (cons h t)) (linsert h (insapp m t)))))
(assert (forall ((m List)) (= (insapp m nil) m)))
(assert (forall ( (a Iint)) (= (tsort  (leaf a)) (cons a nil))))
(assert (forall (  (xl Tree) (xr Tree)) (= (tsort  (bin xl xr)) (insapp (tsort xl) (tsort xr)))))
(assert (not (forall ((var0 Tree)) (= (count (flatten var0)) (counttree var0)))))

(check-sat)
