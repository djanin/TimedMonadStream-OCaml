

open Monad

module type MONADREF = 
  sig
    include MONAD
    type 'a mref
    val fork : 'a m -> ('a mref) m
    val read : 'a mref -> 'a m
  end
    
module IO_Ref (* : MONADREF 
       with type 'a m = 'a io and type 'a mref =  'a MVar.mvar *)
  =
  struct
    include IO
    type 'a mref = 'a MVar.mvar
    let rec fork m
      = let v = MVar.createEmpty () in
        ignore (Thread.create (fun _ -> MVar.put v (run m)) ());
        return v
    let read v = return (MVar.read v)
  end

type ('a ,'b) sum =
  | Left of 'a
  | Right of 'b
let toLeft a = Left a
let toRight b = Right b
                    

let case p f g = match p with
  | (Left a) -> f a
  | (Right b) -> g b
                                     
               
    
module type  MONADREF_PLUS =
  sig
    include MONADREF
    val tryRead : 'a mref -> 'a option m
    val parRead : 'a mref -> 'b mref -> ('a,'b) sum m
  end
    

module IO_Ref_Plus (* : MONADREF_PLUS *) =
  struct
    include IO_Ref
    let tryRead v =
      IO.return (MVar.tryRead v)
    let rec parRead  pl pr  =
      let v = MVar.createEmpty () in
      let tryWrite (p,f) =
        MVar.tryPut v (f (IO.run (read p)))
      in (ignore (Thread.create tryWrite (pl,toLeft));
          ignore (Thread.create tryWrite (pr,toRight)); 
          read v)
  end
    
module type ASYNC = functor (M : MONADREF) -> 
  sig
    type 'a p = 'a M.mref M.m                    
    val return : 'a -> 'a p
    val bind : 'a p -> ('a -> 'b p) -> 'b p
    val fmap : ('a -> 'b) -> 'a p  -> 'b p
    val compose  : ('b -> 'c p) -> ('a -> 'b p) -> 'a -> 'c p
  end

module Async : ASYNC = functor (M : MONADREF) ->
  struct
    type 'a p = 'a M.mref M.m                    
    let return a = M.fork (M.return a)
    let bind m f = M.bind m
      (fun r -> M.fork (M.bind (M.bind (M.read r) f) M.read))
    let fmap f m = bind m (fun r -> return (f r))
    let compose g f
      = fun a -> M.fork(M.bind (M.bind (M.bind (f a) M.read) g) M.read)
  end                                  
