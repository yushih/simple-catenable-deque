module ArrayDeque  = 
  struct
    type queue = int array
    let emtpy = Array.make 0 0
    let isEmpty q = (Array.length q)==0
    let cons x q = Array.concat [[| x |]; q]
    let head q = Array.get q 0
    let tail q = Array.sub q 1
    let snoc q x= Array.concat [q; [| x |]]

    let init q = Array.sub 0 ((Array.length q) - 1)
  end

module CatDeque = SimpleCatenableDeque(ArrayDeque)
