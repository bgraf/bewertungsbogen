


(*---------------------------------------------------------------------------*)

module type Config = sig
  val group_column_count : int
  val title : string
end


(*---------------------------------------------------------------------------*)

module Show(C : Config) : sig
  type t = Task.t
  type handler = string -> unit
  val show : f:handler -> t list -> unit
end



