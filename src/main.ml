open Deque

module ArrayDeque  = 
  struct
    type 'a queue = 'a array
    let empty = [| |]
    let isEmpty q = (Array.length q)==0
    let cons x q = Array.concat [[| x |]; q]
    let head q = Array.get q 0
    let tail q = Array.sub q 1 ((Array.length q) - 1)
    let snoc q x= Array.concat [q; [| x |]]
    let last q = Array.get q ((Array.length q)- 1)
    let init q = Array.sub q 0 ((Array.length q) - 1)
  end

module CatDeque = SimpleCatenableDeque(ArrayDeque)
