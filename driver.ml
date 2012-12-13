(* driver.ml
   Makes things go.
*)

open Sdlkey

let initLight () =
  let light_ambient = 0.5, 0.5, 0.5, 1.0
  and light_diffuse = 1.0, 1.0, 1.0, 1.0
  and light_specular = 1.0, 1.0, 1.0, 1.0
    (*  light_position is NOT default value	*)
  and light_position = 1.0, 1.0, 1.0, 0.0
  in
    List.iter (GlLight.light ~num:0)
      [ `ambient light_ambient; `diffuse light_diffuse;
	`specular light_specular; `position light_position ];

    Gl.enable `lighting;
    Gl.enable `light0;
    Gl.enable `depth_test;
;;


let initGL w h =
  GlDraw.shade_model `smooth;
  (*GlDraw.polygon_mode `both `line;*)
  GlClear.color (0., 0., 0.);
  GlClear.depth 1.0;
  GlClear.clear [`color; `depth];
  Gl.enable `depth_test;
  GlFunc.depth_func `lequal;

  GlMisc.hint `perspective_correction `nicest;

  GlMat.mode `projection;
  GlMat.load_identity ();
  (* Note: this sets the min and max z distances *)
  GluMat.perspective ~fovy: 60. ~aspect: ((float_of_int w) /. (float_of_int h)) ~z: (1., 500.);
  GlMat.mode `modelview;
  GlMat.load_identity ();



  initLight ();

;;



class driver =
object (self)

   val mutable objs : Octopus.onode = Octopus.makeRandomOctree 300
   val mutable continue = true
   val mutable camera = new Camera.camera (100., 0., 0.)
   val mutable namemap = Hashtbl.create 256;


  val mutable lastTick = 0
  val mutable thisTick = 0

  val mutable numFrames = 0

  val mutable nextIndex = 0
  initializer
     Octopus.iter (fun x -> Hashtbl.add namemap nextIndex x; nextIndex <- nextIndex + 1) objs;

  method addObj m = 
     objs <- Octopus.insert m objs;


  method doInput =
     ignore (Sdlevent.pump ());
    if (is_key_pressed !Input.menu) or (is_key_pressed !Input.help) then
      self#stop;

    let c = camera in
    if is_key_pressed !Input.camerapx then
      c#addPhi (0.01);
    if is_key_pressed !Input.cameranx then
      c#addPhi (-0.01);

    if is_key_pressed !Input.camerapy then
      c#addTheta (0.01);
    if is_key_pressed !Input.camerany then
      c#addTheta (-0.01);

    if is_key_pressed !Input.camerapz then
      c#addRadius (0.1);
    if is_key_pressed !Input.cameranz then
      c#addRadius (-0.1);



  method stop =
    continue <- false

  method doDrawing =
        GlClear.clear [`color; `depth]; 
        GlMat.load_identity ();

        let x,y,z = camera#getFocus in
        let cx,cy,cz = camera#getPos in
        let fx = x +. cx
        and fy = y +. cy
        and fz = z +. cz in
        GluMat.look_at ~eye: (fx,fy,fz) ~center: (x,y,z) ~up: (0., 0., 1.);

        Octopus.iter Octopus.drawThingy objs;  
        Octopus.drawTree objs;


        Gl.flush ();

  method mainloop = 
     let t = Octopus.getThingy objs in
     camera#setFocus t.Octopus.t_loc;
     while continue do 

        lastTick <- thisTick;
        thisTick <- Sdltimer.get_ticks ();
        let dt = ((float_of_int (thisTick - lastTick)) /. 1000.) in
        self#doDrawing;
        self#doInput;
        (*
        GlClear.clear [`color; `depth];
        *)
        numFrames <- numFrames + 1;
        Sdlgl.swap_buffers ();
     done; 
     let seconds = (float_of_int thisTick) /. 1000. in
     let fps = (float_of_int numFrames) /. seconds in
	  Printf.printf "FPS: %f\n" fps;
	

end;;
