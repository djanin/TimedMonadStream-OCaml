open Monad
open MonadRef
       
module type STREAM  = functor (M : MONAD)  ->
  sig
    type 'a s = Stream of ('a * 'a s) option M.m
    val exec : unit s -> unit M.m
    (* one should note execute a stream that has values *)
    val fromStream : 'a s -> ('a * 'a s) option M.m
    val toStream : ('a s) M.m -> 'a s
    val fmap : ('a -> 'b) -> 'a s -> 'b s
    val fmapM : ('a -> 'b M.m) -> 'a s -> 'b s
  end
       
module Stream : STREAM = functor (M : MONAD) ->
  struct
    open M
    open M.Syntax       
    type 'a s = Stream of (('a * ('a s)) option) M.m
    let rec exec (Stream m) : unit m
      = let* c = m in match c with
            None -> return ()
	  | Some (a,sc) -> exec sc
    let fromStream (Stream m) = m
    let toStream ms = Stream (ms >>= fromStream)
    let rec fmap f (Stream m) = Stream (
       let* c = m in match c with
         None -> return None
       | Some (a,sc) -> return (Some ((f a),(fmap f sc))))
    let rec fmapM g (Stream m) : 'b s = Stream (
       let* c = m in match c with
          None -> return None
        | Some (a,sc) -> let* b = g a in return (Some (b, fmapM g sc)))          
  end

module SStream (M : MONAD) =
  struct
    open M
    open M.Syntax       
    include Stream(M)
    let rec zip (Stream m1) (Stream m2) = Stream (
       let* c1 = m1 in match c1 with
         None -> return None
       | Some (a1,sc1) -> let*c2 = m2 in match c2 with
            None -> return None
          | Some (a2,sc2) -> return (Some ((a1,a2),zip sc1 sc2)))
    let rec iterate m
      = Stream (let* x = m in return (Some (x, iterate m)))
    let rec iterateSome m
      = Stream (let* c = m in match c with
          None -> return None
        | Some x -> return (Some (x, iterateSome m)))
    let rec loop s f (Stream m)
      = Stream (let* c = m in match c with
          None -> return None
        | Some (a,sc) -> let* (s',b) = f (s,a) in return (Some (b, loop s' f sc)))
    let rec fold b f (Stream m)
      = let* c = m in match c with
          None -> return b
        | Some (a,sc) -> fold (f b a) f sc
  end

    
