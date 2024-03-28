module type t = sig
  val read_utf8 :
    unit -> [ `Read of string | `End | `Retry | `Malformed of string ]
end

let queue = Queue.create ()
let enqueue input = List.iter (fun i -> Queue.push i queue) input

module Virtual : t = struct
  let read_utf8 () =
    if Queue.is_empty queue then `End else `Read (Queue.take queue)
end
