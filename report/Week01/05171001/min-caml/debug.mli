val print_parser: out_channel -> Syntax.t -> unit
type debug_level =
  | Parser
  | Typing
  | KNormal
  | Alpha
  | Closure
  | Virtual
  | Simm
  | RegAlloc
  | Emit
val string_to_debug_level: string -> debug_level
