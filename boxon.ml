
open Gg
open Vg

(* Sheet properties *)

let aspect = sqrt 2.
let page_width = aspect *. 210.
let page_height = 210.
let size = Size2.v page_width page_height (* mm *)
let view = Box2.v P2.o (Size2.v page_width page_height)

let debug_pt pt = Printf.eprintf "(%.2f,%.2f)\n%!" (P2.x pt) (P2.y pt)
let debug_v vec =Printf.eprintf "(%.2f,%.2f)\n%!" (V2.x vec) (V2.y vec)

    
let tuck pt1 pt2 end_pt path =
  let open P in
  let pt1_v = V2.v (P2.x pt1) (P2.y pt1) in
  let pt2_v = V2.(pt1_v + v (P2.x pt2) (P2.y pt2)) in
  let end_pt_v = V2.(pt2_v + v (P2.x end_pt) (P2.y end_pt)) in
  let c = V2.(v (x end_pt_v /. 2.) (y end_pt_v /. 2.)) in
  let to_c vec = V2.(vec - c) in
  let of_c vec = V2.(vec + c) in
  let pt1_c = to_c pt1_v in
  let pt2_c = to_c pt2_v in
  let contract pt_c dist =
    let open V2 in
    let sign proj = if proj pt_c < 0. then -.1. else 1. in
    v (x pt_c -. sign x *. dist) (y pt_c -. sign y *. dist)
    
  in
  let new_pt1_c = contract pt1_c 1. in
  let new_pt2_c = contract pt2_c 1. in
  let new_pt1_v = of_c new_pt1_c in
  let new_pt2_v = of_c new_pt2_c in
  let new_pt1 = P2.v (V2.x new_pt1_v) (V2.y new_pt1_v) in
  let new_pt2 = P2.v (V2.x new_pt2_v -. V2.x new_pt1_v) (V2.y new_pt2_v -. V2.y new_pt1_v) in
  let new_end_pt = P2.v (V2.x end_pt_v -. V2.x new_pt2_v) (V2.y end_pt_v -. V2.y new_pt2_v) in
  let rel = true in
  path >>
  line ~rel new_pt1 >>
  line ~rel new_pt2 >>
  line ~rel new_end_pt
  

(*box dimension in millimeters*)    
let mk_box
    ~width
    ~height
    ~depth
    ~lid_back
    ~lid_front
    =
  let path =
    let open P in
    let open P2 in
    let rel = true in
    let start = v (10. +. depth) (10. +. depth +. height) in
    sub start empty >>
    line ~rel (v (2. *. (width +. depth)) 0.) >>
    line ~rel (v 0. (-. height)) >>
    line ~rel (v (-. 2. *. (width +. depth)) 0.) >>
    line ~rel (v 0. height) >>
    sub ~rel (v width 0.) >>
    line ~rel (v 0. (-.height)) >>
    sub ~rel (v depth 0.) >>
    line ~rel (v 0. height) >>
    sub ~rel (v width 0.) >>
    line ~rel (v 0. (-. height)) >>
    sub ~rel (v depth (height -. lid_back)) >>
    line ~rel (v (-. depth) (lid_back -. lid_front)) >>
    line ~rel (v (-. width) 0.) >>
    line ~rel (v (-. depth) (lid_front -. lid_back)) >>
    sub start >>
    line ~rel (v 0. depth) >>
    line ~rel (v width 0.) >>
    line ~rel (v 0. (-. depth)) >>
    sub start >>
    sub ~rel (v 0. (-. height)) >>
    line ~rel (v 0. (-. depth)) >>
    line ~rel (v width 0.) >>
    line ~rel (v 0.  depth) >>
    sub start >>
    tuck (v (-.depth) 0.) (v 0. (-.lid_front)) (v depth (lid_front -. lid_back)) >>
    tuck (v (-.depth) (lid_back -. lid_front)) (v 0. (lid_front -. height)) (v depth 0.) >>
    sub ~rel (v width 0.) >>
    tuck (v 0. (-. depth)) (v depth 0.) (v 0. depth) >>
    tuck (v 0. (-. depth)) (v width 0.) (v 0. depth) >>
    tuck (v 0. (-. depth)) (v depth 0.) (v 0. depth) >>
    sub ~rel (v 0. height) >>
    tuck (v 0. depth) (v (-.depth) 0.) (v 0. (-. depth)) >>
    tuck (v 0. depth) (v (-. width) 0.) (v 0. (-. depth)) >>
    tuck (v 0. depth) (v (-. depth) 0.) (v 0. (-. depth))
  in
  I.const (Color.v_srgb 0. 0. 0.)
    >> I.cut ~area: (`O {P.o with P.width = 0.1}) path


let mk_inset ~width ~height ~depth ~lid_back =
  let open P in
  let open P2 in
  let path =
    let rel = true in
    sub (v (page_width -. 10.) (page_height -. 10.)) empty >>
    line ~rel (v  (-. (depth +. width /. 5.)) 0.) >>
    line ~rel (v (-. width /. 5.) (-. lid_back)) >>
    line ~rel (v (-. width /. 5.) 0.) >>
    line ~rel (v (-. width /. 5.) lid_back) >>
    line ~rel (v (-. (depth +. width /. 5.)) 0.) >>
    line ~rel (v 0. (-. height)) >>
    line ~rel (v (2. *. depth +. width) 0.) >>
    line ~rel (v 0. height) >>
    sub ~rel (v (-. depth) 0.) >>
    line ~rel (v 0. (-. height)) >>
    sub ~rel (v (-. width) 0.) >>
    line ~rel (v 0. height)
  in
  I.const (Color.v_srgb 0. 0. 0.)
    >> I.cut ~area: (`O {P.o with P.width = 0.1; join =`Round}) path
  

(* 2. Render *)

let () =
  let width = 62. in
  let height = 89. in
  let depth = 12. in
  let lid_back = 10. in
  let lid_front = 25. in
  let box = mk_box ~width ~height ~depth ~lid_back ~lid_front in
  let inset = mk_inset ~width: (width -. 0.5) ~height: (height -. 3.) ~depth: (depth -. 1.5) ~lid_back: (lid_back -. 3.) in
  
  let title = "Lid box template" in
  let description = "Lid box template" in
  let xmp = Vgr.xmp ~title ~description () in
  let warn w = Vgr.pp_warning Format.err_formatter w in
  let r = Vgr.create ~warn (Vgr_pdf.target ~xmp ()) (`Channel stdout) in
  if 5. *. depth +. 3. *. width < page_width -. 20. then
    let image = I.blend box inset in
    ignore (Vgr.render r (`Image (size, view, image)));
  else begin
    ignore (Vgr.render r (`Image (size, view, box)));
    ignore (Vgr.render r (`Image (size, view, inset)))
  end;
  ignore (Vgr.render r `End)