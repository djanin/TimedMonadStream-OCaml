open MonadStream

open Monad
       
open MonadRef

open MonadStreamRef
       
       
module StreamIO = AStreamIO

open StreamIO
open IO
       
let getChar : char io
  = IO (fun () -> input_char stdin)
    
let printChar c : unit io
  = IO (fun () -> print_char c)
       
                          
open IO_Ref_Plus
       
let stdinStream = StreamIO.iterate getChar

let toStdOut s = StreamIO.fmap (fun c -> print_char c) s                             
let toStdOut s = fmapM printChar s                             
                                   
let printStream s
  = run(exec (toStdOut s))

let echo = fun () -> printStream (stdinStream)

                  
let stdinStreamUntil c
  = StreamIO.iterateSome
      (IO.bind getChar
         (fun x -> if (x == c) then
                     IO.return None
                   else
                     IO.return (Some x)))


let print_double_char = fun (c1,c2) ->
  print_char '(' ;print_char c1 ;print_char ',';
  print_char c2 ;print_char ')'
                                            
let toDStdOut s = StreamIO.fmap print_double_char s                                                                
let printDoubleStream s
  = run(exec (toDStdOut s))

(* performance test in IO *)
       
let echoUntil c = printStream (stdinStreamUntil c)

let echoUntilRef c
  = run (forkStream (stdinStreamUntil c)
         >>= fun r -> return(readStream r)
         >>= fun s -> exec (toStdOut s))

       
let echoUntilRefZip c
  = run (forkStream (stdinStreamUntil c)
         >>= fun r -> return(zip (readStream r) (readStream r))
         >>= fun s -> exec (toDStdOut s))

                   

let _ : unit
  = echoUntilRefZip 'q'
                    (* echoUntil 'q' *) (* runs at more than 300 kHz *)
                    (* echoUntilRef 'q' *) (* runs at about 10 kHz *)
                    (* echoUntilRefZip 'q' *) (* runs at about 1.4 kHz with memory leak *)
            

                                         
