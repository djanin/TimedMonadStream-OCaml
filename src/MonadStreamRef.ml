
open MonadStream       

module StreamRef (M : MonadRef.MONADREF)               
  =
  struct
    open M
    open M.Syntax       
    include SStream(M)       
    type 'a sref = StreamRef of (('a * ('a sref)) option) mref
    let forkStream (s : 'a s) : 'a sref m =
      let rec evalAndFork (Stream m)
        = let* c = m in match c with
               None -> return None
             | Some (a,sc) -> let* r = fork (evalAndFork sc) in
                              return (Some (a, StreamRef r))
      in let* r = fork (evalAndFork s) in return (StreamRef r)
    let rec readStream (StreamRef r)
      = Stream (let* c = read r in match c with
                None -> return None
              | Some (a,rc) ->
                 return (Some (a, readStream rc)))           
  end

open Monad    
open MonadRef       
    
module StreamRef_Plus  (M : MONADREF_PLUS)  
  =
  struct
    include StreamRef(M)              
    let fmapStream = fmap                 
    open M
    open M.Syntax       
    let rec mergeStreamRef (StreamRef r1) (StreamRef r2)
      = Stream (let* c = parRead r1 r2 in match c with
            Left None -> fromStream (readStream (StreamRef r2))
          | Left (Some(a1,sc)) -> return (Some (a1, mergeStreamRef sc (StreamRef r2)))
          | Right None -> fromStream (readStream (StreamRef r1))
          | Right (Some(a2,sc)) -> return (Some (a2, mergeStreamRef (StreamRef r1) sc)))
    let rec merge s1 s2
      = toStream (let* r1 = forkStream s1 in
                  let* r2 = forkStream s2 in
                  return (mergeStreamRef r1 r2))
    let mergeStream s1 s2
      = merge (fmapStream toLeft s1) (fmapStream toRight s2)      
  end

module MonadStream_Core (M : MONADREF_PLUS) : MONAD_CORE =
  struct
    open StreamRef_Plus (M)
    type 'a m = 'a s                    
    let return a
      = let empty = Stream (M.return None) in 
        Stream (M.return (Some (a, empty)))
    let rec bind (Stream m) f
      = toStream (M.bind m (function
            | None -> M.return (Stream (M.return None))
            | Some (a , sc) -> M.return (merge (f a) (bind sc f))))
  end

module StreamIORef = StreamRef (IO_Ref_Plus)
    
    
module MonadStream (M : MONADREF_PLUS)
  = Monad (MonadStream_Core (M))
      
module AStream (M : MONADREF_PLUS) =
  struct
    include StreamRef_Plus(M)
    open M 
    open M.Syntax       
    let rec asyncMapRef f (StreamRef mf) s =
      let applyAndCont f sf (Stream m) = let* c = m in match c with
          Some(a,sc) -> return (Some (f a , asyncMapRef f sf sc))
        | None -> return None in
      Stream (let* c = tryRead mf in match c with 
          None -> applyAndCont f  (StreamRef mf) s
        | Some (None) -> fromStream (fmapStream f s)
        | Some (Some(f1 , scf)) -> applyAndCont f1 scf s)
    let  asyncMap f sf s =  toStream
       (let* r = forkStream sf in return(asyncMapRef f r s))           
  end
    
module AStreamIO =
  struct
    include StreamRef_Plus(IO_Ref_Plus)
    open IO_Ref_Plus                      
    let choice listRef =
      let n = List.length listRef in
      let v = MVar.createEmpty () in 
      let concurrentWrite r = MVar.put v (run (read r)) in
      let rec sequentialRead m
        =  if (m==0) then return None
           else let a = MVar.read v
                in return (Some (a, Stream (sequentialRead (m-1))))
      in (ignore (List.map (fun r ->
                      ignore (Thread.create concurrentWrite r)) listRef);
          return (Stream (sequentialRead n)))
  end
         
  
                      
                         
