open Minttea_internal

let queue = Queue.create ()
let enqueue input = List.iter (fun i -> Queue.push i queue) input

module Virtual = struct
  let read_utf8 () =
    if Queue.is_empty queue then `End else `Read (Queue.take queue)
end

let pp_key key =
  match key with
  | Event.Up -> "up"
  | Event.Down -> "down"
  | Event.Left -> "left"
  | Event.Right -> "right"
  | Event.Space -> "space"
  | Event.Enter -> "enter"
  | Event.Escape -> "escape"
  | Event.Backspace -> "backspace"
  | _ -> "Unknowna"

let pp e =
  match e with
  | Some (Event.KeyDown (key, modifier)) -> (
      match (key, modifier) with
      | Event.Up, Event.No_modifier -> "<up>"
      | Event.Key c, Event.Ctrl -> "<c-" ^ c ^ ">"
      | _ -> "Unknowns")
  | _ -> "Unknown"

let test_ctrl_keys () =
  enqueue
    [
      "\x01";
      "\x02";
      "\x03";
      "\x04";
      "\x05";
      "\x06";
      "\x07";
      "\x08";
      "\x09";
      "\x0a";
      "\x0b";
      "\x0c";
      "\x0d";
      "\x0e";
      "\x0f";
      "\x10";
      "\x11";
      "\x12";
      "\x13";
      "\x14";
      "\x15";
      "\x16";
      "\x17";
      "\x18";
      "\x19";
      "\x1a";
    ];
  Alcotest.(check (list string))
    "capture ctrl-c"
    [
      "<c-a>";
      "<c-b>";
      "<c-c>";
      "<c-d>";
      "<c-e>";
      "<c-f>";
      "<c-g>";
      "<c-h>";
      "<c-i>";
      "<c-j>";
      "<c-k>";
      "<c-l>";
      "<c-m>";
      "<c-n>";
      "<c-o>";
      "<c-p>";
      "<c-q>";
      "<c-r>";
      "<c-s>";
      "<c-t>";
      "<c-u>";
      "<c-v>";
      "<c-w>";
      "<c-x>";
      "<c-y>";
      "<c-z>";
    ]
    (List.map pp (List.init 26 (fun _ -> Io_loop.parse_utf8 (module Virtual))))

let run = [ Alcotest.test_case "capture ctrl+keys" `Quick test_ctrl_keys ]
