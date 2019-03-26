module type Deque = 
  sig
    type 'a queue
    val empty : 'a queue
    val isEmpty: 'a queue -> bool
                               
    val cons: 'a -> 'a queue -> 'a queue
    val head: 'a queue -> 'a
    val tail: 'a queue -> 'a queue

    val snoc: 'a queue -> 'a -> 'a queue
    val last: 'a queue -> 'a
    val init: 'a queue -> 'a queue

  end

module SimpleCatenableDeque(D : Deque)  = 
    struct 
      type 'a lazy_repr = Lazy_tail_force of 'a lazy_repr
                        | Lazy_cons_f_force_m of ('a D.queue) * ('a lazy_repr)
                        | Lazy_snoc_force_m_r of ('a lazy_repr) * ('a D.queue)
                        | Lazy_snoc_ of ('a lazy_repr) * ('a D.queue) * ('a D.queue) * ('a lazy_repr)
                        | Lazy_empty

      type ('s, 'a) susp = Susp of (unit->'s) * ('a lazy_repr)

      type 'a cat = Shallow of 'a D.queue
                  | Deep of ('a D.queue) * ((('a D.queue cat), 'a) susp) * ('a D.queue)

      let force x =  match x with Susp(lambda, _) -> lambda()
      let repr x = match x with Susp(_, r) -> r
                                                                    
      let tooSmall d = D.isEmpty d || D.isEmpty (D.tail d)

      let dappendL d1 d2 = 
        if D.isEmpty d1 then d2 else D.cons (D.head d1) d2

      let dappendR d1 d2 = 
        if D.isEmpty d2 then d1 else D.snoc d1 (D.head d2)

      let empty = Shallow (D.empty)
      let isEmpty = function Shallow(d) -> D.isEmpty d
                           | _ -> false

      let cons x d = match d with 
        | Shallow(d) -> Shallow(D.cons x d)
        | Deep(f, m, r) -> Deep((D.cons x f), m, r)

      let snoc d x = match d with
        | Shallow(d) -> Shallow(D.snoc d x)
        | Deep(f, m, r) -> Deep(f, m, (D.snoc r x))

      let head = function Shallow(d) -> D.head d
                        | Deep(f, _m, _r) -> D.head f

      let last = function Shallow(d) -> D.last d
                        | Deep(_f, _m, r) -> D.last r

      let rec tail : 'a. 'a cat -> 'a cat = function
        |Shallow(d) -> Shallow (D.tail d)
        |Deep(f, m ,r) ->
          let ft = D.tail f in
          if not (tooSmall ft) then Deep(ft, m, r) 
          else if isEmpty (force m) then Shallow(dappendL ft r)
          else Deep((dappendL ft (head (force m))),
                    Susp((fun ()->(force m)), Lazy_tail_force(repr m)),
                    r)

      let rec concat : 'a. 'a cat -> 'a cat ->'a cat = fun deq1 deq2 ->
        match deq1, deq2 with
        |Shallow(d1), Shallow(d2) -> 
          if tooSmall d1 then Shallow(dappendL d1 d2) 
          else if tooSmall d2 then Shallow(dappendR d1 d2)
          else Deep(d1,
                    Susp((fun ()->empty), Lazy_empty),
                    d2)
        |Shallow(d), Deep(f, m, r) ->
          if tooSmall d then Deep((dappendL d f), m, r)
          else Deep(d, 
                    Susp((fun ()->cons f (force m)), Lazy_cons_f_force_m(f, repr m)),
                    r)
        |Deep(f, m, r), Shallow(d) ->
          if tooSmall d then Deep(f, m, (dappendR r d))
          else Deep(f,
                    Susp((fun ()->snoc (force m) r), Lazy_snoc_force_m_r(repr m, r)),
                    d)
        |Deep(f1, m1, r1), Deep(f2, m2, r2) ->
          Deep(f1,
               Susp((fun ()->concat
                               (snoc (force m1) r1) 
                               (cons f2 (force m2))),
                    Lazy_snoc_(repr m1, r1, f2, repr m2)),
               r2)
                   
    end



