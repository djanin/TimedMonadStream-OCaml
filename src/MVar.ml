(*
Copyright (C) 2017 John Else

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights to
use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies
of the Software, and to permit persons to whom the Software is furnished to do
so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
  SOFTWARE.

 *)

(* sligtly modified by David Janin 2019 *)

module Mutex = struct
  include Mutex
  (** execute the function f with the mutex hold *)
  let execute lock f =
    Mutex.lock lock;
    let r = begin try f () with exn -> Mutex.unlock lock; raise exn end; in
    Mutex.unlock lock;
    r
end

type 'a mvar = {
  data: 'a option ref;
  m: Mutex.t;
  c: Condition.t;
}

let createEmpty () =
  {
    data = ref None;
    m = Mutex.create ();
    c = Condition.create ();
  }

let create x =
  {
    data = ref (Some x);
    m = Mutex.create ();
    c = Condition.create ();
  }

let take mvar =
  let rec check () =
    match !(mvar.data) with
    | None ->
      Condition.wait mvar.c mvar.m;
      check ()
    | Some x ->
      mvar.data := None;
      Condition.signal mvar.c;
      x
    in
  Mutex.execute mvar.m (fun () -> check ())

let tryTake mvar =
  Mutex.execute mvar.m (fun () ->
    match !(mvar.data) with
    | None -> None
    | Some x ->
      mvar.data := None;
      Condition.signal mvar.c;
      Some x)

let put mvar x =
  let rec check () =
    match !(mvar.data) with
    | None ->
      mvar.data := (Some x);
      Condition.signal mvar.c
    | Some _ ->
      Condition.wait mvar.c mvar.m;
      check ()
  in
  Mutex.execute mvar.m (fun () -> check ())

let tryPut mvar x =
  Mutex.execute mvar.m (fun () ->
    match !(mvar.data) with
    | Some _ ->
      false
    | None ->
      mvar.data := (Some x);
      Condition.signal mvar.c;
      true)

let read mvar =
  let rec check () =
    match !(mvar.data) with
    | None ->
      Condition.wait mvar.c mvar.m;
      check ()
    | Some x ->
       (* Condition.signal mvar.c; (* c'est Simon qui le dit *) *) 
      x
    in
  Mutex.execute mvar.m (fun () -> check ())

let tryRead mvar =
  Mutex.execute mvar.m (fun () ->
    match !(mvar.data) with
    | None -> None
    | Some x ->
       (* Condition.signal mvar.c; (* c'est Simon qui le dit *) *) 
      Some x)

    
