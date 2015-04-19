
open Batteries

module type Config = sig
  val group_column_count : int
  val title : string
end

(*---------------------------------------------------------------------------*)

(** Hierachic identifier module *)

module type Id_Elt = sig
  type t
  val one : t
  val succ: t -> t
  val to_string : t -> string
end

module type Id_Intf = sig
  type t
  type elt
  val from : elt -> t
  val sibling : t -> t
  val child : t -> t
  val append : t -> elt -> t
  val to_string : t -> string
end

module Make_Id (X:Id_Elt)
  : Id_Intf with type elt := X.t =
struct
  type t = X.t list

  let from e = [e]

  let sibling = function
    | x::xs -> (X.succ x)::xs
    | [] -> failwith "cannot happen"

  let child xs = X.one::xs

  let append xs e = e::xs

  let to_string xs =
    List.rev_map X.to_string xs
    |> String.concat "." 
  
end

(*---------------------------------------------------------------------------*)

module Show(C : Config) = struct
  type t = Task.t
  type handler = string -> unit

  module H = Html5.M
  module P = Html5.P
  module Html5 = Html5.M
  
  module Id = Make_Id(struct
    type t = int
    let one = 1
    let succ = succ
    let to_string = string_of_int
  end)

  (*----------------------------------*)

  let style = 
    <:html5< <style>
      body {
        font-family:tahoma;
        font-size:8pt;
        background:#ffffff;
      }
      h1 {
        color:black;
        background:#dddddd;
        padding:3pt;
      }
      table { width:100%; border-collapse:collapse; }
      td {
        border:1pt solid #cccccc;
        padding:1pt;
        padding-left:3pt;
      }
      tr.group { border:1pt solid #cccccc; }
      tr.group td { font-weight:bold; border:0; }
      tr.level0 td { background:#aaaaaa; }
      tr.level1 td { background:#dddddd; }
      tr.levelN td { background:#ffffff; }
      tr.score td { height:12pt; }
      tr.hline td { height:0pt; background:#cccccc; }
      td.name { width:1pt; white-space:nowrap; padding-left:3pt; padding-right:3pt; }
      td.points { text-align:right; width:1pt; padding-right:5pt; }
      tr.heading td { font-weight:bold; font-size:11pt; }
      td.group_name { text-align:center; font-size:8pt;  }
    </style> >>

  (*----------------------------------*)
  
 
  let group_cells =
    List.make
      C.group_column_count
      (<:html5< <td></td> >>)

  let group_cells_for_task { Task.kind; } = match kind with
    | Task.Group _ ->
      [ <:html5< <td colspan=$C.group_column_count |> string_of_int$></td> >> ]
    | _ -> group_cells
     
  let score_row ?(css="score") ?(info = "") () =
    <:html5<
      <tr class=$css$>
        <td colspan="2"><b>$str: info$</b></td>
        $list: group_cells$
      </tr>
    >>

  let final_score_row ~score =
    <:html5<
      <tr class="score">
        <td><b>Gesamt</b></td>
        <td class="points">$str: score |> string_of_int$</td>
        $list: group_cells$
      </tr>
    >>

  let class_for_level = function
    | n when n < 2 -> "level" ^ (string_of_int n)
    | _ -> "levelN"

  let class_for_task ~level task =
    if Task.is_group task then
      let level_class = class_for_level level in
      "group " ^ level_class
    else ""

  let string_of_id id =
    Printf.sprintf "%s: " (Id.to_string id)

  let rec rows_of_task ?(level = 0) ~accf ~id task =
    let row =
      let name =
        let info =
          if level == 0 || Task.is_group task
          then string_of_id id else "" in
        info ^ task.Task.name
      and points = Task.points task |> string_of_int in
      let task_cells = 
        [ <:html5< <td class="name">$str: name$</td> >>;
          <:html5< <td class="points">$str: points$</td> >> ]
      in
      let classes = class_for_task ~level task in
      <:html5< <tr class=$classes$>$list:task_cells$ $list:group_cells_for_task task$</tr> >>
    in
    
    let () = accf row in (* accumulate row *)

    begin match task.Task.kind with
    | Task.Group subtasks ->
        let () = subtasks |> List.iteri
          (fun i -> rows_of_task ~accf ~level:(level+1) ~id:(Id.append id (i+1)))
        in
        accf begin if level == 0
          then score_row ()
          else score_row ~css:"hline" ()
        end
    | _ -> ()
    end


  let rows_of_tasks tasks =
    let rows_r = ref [] in
    let () =
      List.iteri
        (fun i -> rows_of_task
          ~accf:(fun r -> rows_r := r::!rows_r)
          ~level:0
          ~id:(Id.from (i+1)))
        tasks in
    List.rev !rows_r

  let groupname_of_int i =
    let str = string_of_int i in
    if String.length str > 1 then str
    else "&nbsp;" ^ str

  let table_heading =
    let group_name_cells =
      List.init
        C.group_column_count
        (fun i -> <:html5< <td class="group_name">$str: groupname_of_int (i+1)$</td> >>)
    in
    <:html5<
      <tr class="heading">
        <td colspan="2">$str: C.title$</td>
        $list: group_name_cells$
      </tr>
    >>


  (*----------------------------------*)

  let show ~f tasks =
    let rows = rows_of_tasks tasks in

    let doc = <:html5<
      <html>
        <head>$style$</head>
        <body>
          <table>
            $table_heading$
            $list: rows$
            $final_score_row ~score:(tasks |> List.map Task.points |> List.fold_left (+) 0)$
          </table>
        </body>
      </html>
    >> in

    P.print ~output:f doc

end

