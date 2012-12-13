(* camera.ml
   A camera!
   It stays focussed on an object, and rotates and moves around it
   using polar coordinates.
*)

type vector = float * float * float


class camera focusloc =
object (self)

  val mutable focus : float * float * float = focusloc
  val mutable theta = 0.0
  val mutable phi = 1.0
  val mutable radius = 10.0

  (* A bit of premature optimization, but it does't actually
     change anything, anywhere else.  So. *)
  val mutable realPosition = (0., 0., 0.)

  method private setRealPosition = 
    let x = radius *. (cos theta) *. (sin phi)
    and y = radius *. (sin theta) *. (sin phi)
    and z = radius *. (cos phi) in
      realPosition <- (x, y, z)

  method getPos = realPosition
  method getRelativePos = realPosition

  method setTheta t = theta <- t; self#setRealPosition
  method setPhi p = phi <- p; self#setRealPosition
  method setRadius r = radius <- r; self#setRealPosition

  method set t p r =
    theta <- t;
    phi <- p;
    radius <- r;
    self#setRealPosition

  method setFocus f = focus <- f; self#setRealPosition

  method addTheta t = self#setTheta (theta +. t)
  method addPhi p = self#setPhi (phi +. p)
  method addRadius r = self#setRadius (radius +. r)
  method getFocus = focus

  method get = (theta, phi, radius)

  method toString =
    Printf.sprintf "Camera: %f, %f, %f" theta phi radius;

end;;
