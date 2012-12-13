(* Octopus
   A test in the art of doing rendering culling using octrees.
*)

type loc = float * float * float

(* The things we draw *)
type thingie = {
   t_color : float * float * float;
   t_radius : float;
   t_loc : loc;
};;


(* Octree node *)
type onode =
(*                   xyz    xy-z    x-yz    x-y-z   x-y-z    -xyz   -xy-z   -x-y-z *)
 | Node of thingie * onode * onode * onode * onode * onode * onode * onode * onode
 | Null
;;

let rec insert item = function
   Null -> Node( item, Null, Null, Null, Null, Null, Null, Null, Null )

   (* Talk about unhelpful variable names. *)
 | Node( t, a, b, c, d, e, f, g, h ) ->
   let x1,y1,z1 = item.t_loc
   and x2,y2,z2 = t.t_loc in
   if x1 >= x2 then
      if y1 >= y2 then
         if z1 >= z2 then
            Node( t, (insert item a), b, c, d, e, f, g,  h )
         else
            Node( t, a, (insert item b), c, d, e, f, g, h )
      else
         if z1 >= z2 then
            Node( t, a, b, (insert item c), d, e, f, g, h )
         else
            Node( t, a, b, c, (insert item d), e, f, g, h )
   else
      if y1 >= y2 then
         if z1 >= z2 then
            Node( t, a, b, c, d, (insert item e), f, g, h )
         else
            Node( t, a, b, c, d, e, (insert item f), g, h )
      else
         if z1 >= z2 then
            Node( t, a, b, c, d, e, f, (insert item g), h )
         else
            Node( t, a, b, c, d, e, f, g, (insert item h) )
;;

let rec search loc = function
   Null -> None
 | Node( t, a, b, c, d, e, f, g, h ) ->
   if loc = t.t_loc then
      Some( t )
   else
      let x1,y1,z1 = loc
      and x2,y2,z2 = t.t_loc in
      if x1 >= x2 then
         if y1 >= y2 then
            if z1 >= z2 then
               search loc a
	    else
               search loc b
         else
            if z1 >= z2 then
               search loc c
            else
               search loc d
      else
         if y1 >= y2 then
            if z1 >= z2 then
               search loc e
            else
               search loc f
         else
            if z1 >= z2 then
               search loc g
            else
               search loc h
;;

let rec iter fn = function
   Null -> ()
 | Node( t, a, b, c, d, e, f, g, h ) ->
      fn t;
      iter fn a;
      iter fn b;
      iter fn c;
      iter fn d;
      iter fn e;
      iter fn f;
      iter fn g;
      iter fn h;
;;

let rec map fn = function
   Null -> Null
 | Node( t, a, b, c, d, e, f, g, h ) ->
      Node( fn t, map fn a, map fn b, map fn c, map fn d,
                  map fn e, map fn f, map fn g, map fn h )
;;

let flatten oct =
   let rec loop oct accm =
      match oct with
         Null -> accm
       | Node( t, a, b, c, d, e, f, g, h ) ->
         t :: (loop a (loop b (loop c (loop d (loop e (loop f (loop g (loop h
accm))))))))
   in
      loop oct []
;;



let randomThingy maxx maxy maxz maxr = {
   t_color = (Random.float 1., Random.float 1., Random.float 1.);
   t_radius = (Random.float maxr);
   t_loc = (Random.float maxx, 
            Random.float maxy,
            Random.float maxz);
};;


let makeRandomOctree n =
   let rec loop n accm = 
      if n = 0 then
         accm
      else
         loop (n - 1) (insert (randomThingy 20. 20. 20. 0.5) accm)
   in
      loop n Null
;;

let drawThingy t =
  GlMat.push ();
   let x,y,z = t.t_loc in
   let r,g,b = t.t_color in
   GlMat.translate ~x ~y ~z ();
   GlLight.material ~face: `front (`specular (r, g, b, 1.));
   GlLight.material ~face: `front (`diffuse (r, g, b, 1.));
   GlLight.material ~face: `front (`ambient (r/.10., g/.10., b/.10., 1.));
   GlLight.material ~face: `front (`shininess 5.);
   GluQuadric.sphere ~radius: t.t_radius ~slices: 8 ~stacks: 8 ();
  GlMat.pop ();
;;

let getThingy = function
   Null -> raise (Failure "getThingy")
 | Node( t, _, _, _, _, _, _, _, _ ) -> t
;;

let drawLine a b =
  GlMat.push ();
   Gl.disable `lighting;
   GlDraw.begins `lines;
   GlDraw.color (0., 1., 0.);
   GlDraw.vertex3 a;
   GlDraw.color (1., 0., 0.);
   GlDraw.vertex3 b;
   GlDraw.ends ();
   Gl.enable `lighting;
  GlMat.pop ();
;;

let doWithThingy fn = function
   Null -> ()
 | Node( t, _, _, _, _, _, _, _, _ ) -> fn t

let rec drawTree = function
   Null -> ()
 | Node( t, a, b, c, d, e, f, g, h ) ->
       let draw = (fun x -> drawLine t.t_loc x.t_loc) in
       doWithThingy draw a;
       doWithThingy draw b;
       doWithThingy draw c;
       doWithThingy draw d;
       doWithThingy draw e;
       doWithThingy draw f;
       doWithThingy draw g;
       doWithThingy draw h;
       drawTree a;
       drawTree b;
       drawTree c;
       drawTree d;
       drawTree e;
       drawTree f;
       drawTree g;
       drawTree h;
;;
