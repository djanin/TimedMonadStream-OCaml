module type MONAD_CORE =
  sig
    type 'a m
    val return : 'a -> 'a m
    val bind  : 'a m -> ('a -> 'b m) -> 'b m
  end    

                                               
module type MONAD =
  sig
    include MONAD_CORE
    val (>>=) : 'a m -> ('a -> 'b m) -> 'b m
    val (>>) : 'a m -> 'b m -> 'b m
    val fmap : ('a -> 'b) -> 'a m -> 'b m
    val join : 'a m m -> 'a m
    module Syntax : sig
      val ( let* ) : 'a m -> ('a -> 'b m ) -> 'b m
    end                    
  end    

module Monad (M: MONAD_CORE) : MONAD
       with type 'a m = 'a M.m
  =
  struct
    include M         
    module Syntax = struct          
      (* the ocaml syntax for do-notation *)
      let (let*) x f = bind x f
    end
    open Syntax
    let fmap f m = let* x = m in return (f x)
    let join m = let* x = m in x
    let (>>=) = bind
    let (>>) m1 m2 = bind m1 (fun _ -> m2)
  end

type 'a io = IO of (unit -> 'a)
(**  type of IO monad action *)


module IO_CORE : MONAD_CORE
     with type 'a m = 'a io
  =
  struct
    type 'a m = 'a io
    let return a = IO (fun () -> a)
    let bind (IO g) f
      = IO (fun () -> let a = g () in let IO m = f a in m ())
  end    
    

module type MONAD_IO =
  sig
    include MONAD
    val run : 'a m ->  'a          
    (** forces the execution of a IO monad action *)                     
  end

module IO : MONAD_IO
       with type 'a m = 'a io
  =
  struct
    include Monad (IO_CORE)
    let run (IO s) = s ()
  end

