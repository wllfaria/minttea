type t =
  | Noop
  | Quit
  | Exit_alt_screen
  | Enter_alt_screen
  | Seq of t list
  | Set_timer of unit Riot.Ref.t * float
