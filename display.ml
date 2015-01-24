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
		|11 -> (edgeDate, edgeDown+digit_height+sepDateTime)
		|10->(edgeDate + 1*digit_width +space + 0*sepSlash, edgeDown+digit_height+sepDateTime)
		|9->(edgeDate + 2*digit_width +space + 1*sepSlash, edgeDown+digit_height+sepDateTime)
		|8->(edgeDate + 3*digit_width +2*space + 1*sepSlash, edgeDown+digit_height+sepDateTime)
		|7->(edgeDate + 4*digit_width +2*space + 2*sepSlash, edgeDown+digit_height+sepDateTime)
		|_->(edgeDate + 5*digit_width +3*space + 2*sepSlash, edgeDown+digit_height+sepDateTime)
		
		(*returns the coordinates of digit i in the graphical window*)
(*
let update () =
	let digits = Array.make nb_digits [||];
	let change = Array.make nb_digits false;
	for i = 0 to nb_digits-1 do
		digits.(i)<-Array.make 7 false
	done;
	while true do
		Condition.wait Shared_memory.c Shared_memory.m_out;
		for i = 0 to nb_digits-1 do
			let digit = Array.copy Shared_memory.digits_RAM in
			if digits.(i) = digit then () 
			else begin
				digits.(i)<-digit;
				change.(i)<-true;
			end
		done;
		Mutex.unlock Shared_memory.m_out;
		for i = 0 to nb_digits-1 do
			if change.(i) then begin
				change.(i)<-false;
				display_digit (coords i) digits.(i)
				end
		done
	done
*)		


let display_digit (x,y) (*bottom left coordinates*) (segments :  bool array) =
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

let display_colon (x,y) = 
  set_color foreground;
  fill_rect (x+((sepColon-4)/2)) (y+(digit_height/4)) 4 4;
  fill_rect (x+((sepColon-4)/2)) (y+(3*(digit_height/4))) 4 4

let display_slash (x,y) = 
  set_color foreground;
  fill_poly [|(x+2,y); (x+4, y); (x+sepSlash-2, y+digit_height); (x+sepSlash-4, y+digit_height)|]

let () = 
  open_graph "";
  set_color background;
  fill_rect 0 0 (size_x()) (size_y()); 
  display_colon ((fst (coords 5))+2*digit_width+space, snd(coords 5));
  display_colon ((fst (coords 3))+2*digit_width+space, snd(coords 3));
  display_slash ((fst (coords 11))+2*digit_width+space, snd(coords
  11));
  display_slash ((fst (coords 9))+2*digit_width+space, snd(coords
  11));
  display_digit (coords 5) [|true;true; true; true; true; true;false|];
  display_digit (coords 4) [|true;true; true; true; true; true;false|];
  display_digit (coords 3) [|true;true; true; true; true; true;false|];
  display_digit (coords 2) [|true;true; true; true; true; true;false|];
  display_digit (coords 1) [|false;true; true;false; false; true;true|];
  display_digit (coords 0) [|true;true; false; true; true; false;true|];
  display_digit (coords 11) [|true;true; true; true; true; true;false|];
  display_digit (coords 10) [|true;true; true; true; true; true;false|];
  display_digit (coords 9) [|true;true; true; true; true; true;false|];
  display_digit (coords 8) [|true;true; true; true; true; true;false|];
  display_digit (coords 7) [|false;true; true;false; false; true;true|];
  display_digit (coords 6) [|true;true; false; true; true; false;true|];
  while true do Unix.sleep 50 done

