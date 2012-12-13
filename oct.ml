open Driver;;


let _ = 
  Sdl.init [`EVERYTHING]; 
  let _ = Sdlvideo.set_video_mode ~w: 800 ~h: 600 
    ~bpp: 16 [`DOUBLEBUF; `OPENGL] in 
    initGL 800 600; 

    Random.self_init ();
    let driver = new driver in
      driver#mainloop;

    Sdl.quit (); 
;;
