open Graphics


let nb_digits = 12
let digit_height = 80
let digit_width = 30
let em = 5
let vert_seg_length = (digit_height/2)-em
let hor_seg_length = digit_width -(2*em)
let space = 2*em
let sepColon = 20
let sepSlash=20
let edgeTime = (300-(6*digit_width + 2*sepColon))/2
let edgeDate = (300-(6*digit_width + 2*sepSlash))/2
let sepDateTime = 30
let edgeDown = 40
let background=black
let foreground = green
let underlineSpace = 2

let horizontal x y (*leftmost point*) = fill_poly [| (x,y); (x+em, y+em); (x+hor_seg_length-em, y+em); (x+hor_seg_length, y); (x+hor_seg_length-em, y-em); (x+em, y-em)|]

let vertical x y (*bottommost point*) = fill_poly [| (x,y); (x-em,
							     y+em);
						     (x-em,
						      y+vert_seg_length-em);
						     (x,
						      y+vert_seg_length);
						     (x+em,
						      y+vert_seg_length-em);
						     (x+em, y+em)|]

let coords  = function
		|5 -> (edgeTime, edgeDown)
		|4 -> (edgeTime + 1*digit_width + space, edgeDown)
		|3 -> (edgeTime + 2*digit_width + space + 1*sepColon, edgeDown)
		|2 -> (edgeTime + 3*digit_width +2*space + 1*sepColon, edgeDown)
		|1 -> (edgeTime + 4*digit_width +2*space + 2*sepColon, edgeDown)
		|0 -> (edgeTime + 5*digit_width +3*space + 2*sepColon, edgeDown)
		|7 -> (edgeDate, edgeDown+digit_height+sepDateTime)
		|6->(edgeDate + 1*digit_width +space + 0*sepSlash, edgeDown+digit_height+sepDateTime)
		|9->(edgeDate + 2*digit_width +space + 1*sepSlash, edgeDown+digit_height+sepDateTime)
		|8->(edgeDate + 3*digit_width +2*space + 1*sepSlash, edgeDown+digit_height+sepDateTime)
		|11->(edgeDate + 4*digit_width +2*space + 2*sepSlash, edgeDown+digit_height+sepDateTime)
		|_->(edgeDate + 5*digit_width +3*space + 2*sepSlash, edgeDown+digit_height+sepDateTime)
		
		(*returns the coordinates of digit i in the graphical window*)

let dragdown (i,j) = (i, j-underlineSpace)
let dragdownright (i,j) = (i+digit_width, j-underlineSpace)

let display_digit (x,y) (*bottom left coordinates*) (segments :  bool
						       array)  =
	set_color background;
	fill_rect x y digit_width digit_height;
	set_color foreground;
	if segments.(0) then horizontal (x+em) (y+digit_height-em);
	if segments.(1) then vertical (x+digit_width-em) (y+(digit_height/2));
	if segments.(2) then vertical (x+digit_width-em) (y+em);
	if segments.(3) then horizontal (x+em) (y+em);
	if segments.(4) then vertical (x+em) (y+em);
	if segments.(5) then vertical (x+em) (y+(digit_height/2));
	if segments.(6) then horizontal (x+em) (y+(digit_height/2));
	()

let display_setting digits =
  let disp_to_coords = [|(0,0); (1, 0); (3, 2); (5, 4); (11,10); (9,8); (7,6)|] in
  set_color background;
  draw_poly_line [|dragdown (coords 7); dragdownright (coords
  10)|];
  draw_poly_line [|dragdown (coords 5); dragdownright (coords
  0)|];
  set_color foreground;
  let n = Netlist_utilities.bits_to_int digits in
  match n with 
    |0->()
    |_->let (i,j) = disp_to_coords.(n) in draw_poly_line [|dragdown (coords i); dragdownright (coords j) |]

let display_colon (x,y) = 
  set_color foreground;
  fill_rect (x+((sepColon-4)/2)) (y+(digit_height/4)) 4 4;
  fill_rect (x+((sepColon-4)/2)) (y+(3*(digit_height/4))) 4 4

let display_slash (x,y) = 
  set_color foreground;
  fill_poly [|(x+2,y); (x+4, y); (x+sepSlash-2, y+digit_height); (x+sepSlash-4, y+digit_height)|]


let update () =
  let digits = Array.make (nb_digits+1) [||] in
  let change = Array.make (nb_digits+1) false in
  for i = 0 to nb_digits-1 do
    digits.(i)<-Array.make 7 false
  done;
  while true do
    Condition.wait Shared_memory.c Shared_memory.m_out;
    for i = 0 to nb_digits do
      let digit = Array.copy Shared_memory.digits_RAM.(i) in
      if digits.(i) = digit then () 
      else begin
	digits.(i)<-digit;
	change.(i)<-true;
      end
    done;
    Mutex.unlock Shared_memory.m_out;
    for i = 0 to nb_digits-1 do
      if change.(i) then 
	begin
	  change.(i)<-false;
	  display_digit (coords i) digits.(i)
	    
	end
    done;
    display_setting digits.(nb_digits)
  done

let () = 
  open_graph "";
  set_color background;
  fill_rect 0 0 (size_x()) (size_y()); 
  display_colon ((fst (coords 5))+2*digit_width+space, snd(coords 5));
  display_colon ((fst (coords 3))+2*digit_width+space, snd(coords 3));
  display_slash ((fst (coords 7))+2*digit_width+space, snd(coords
  11));
  display_slash ((fst (coords 9))+2*digit_width+space, snd(coords
							     11)) ;
  moveto edgeTime ((size_y())-edgeDown);
  draw_string "T to autoset time, Enter to run, P to pause.";
  moveto edgeTime ((size_y())-edgeDown-12);
  draw_string "While in pause : S to change speed, Tab/space to set time manually"

