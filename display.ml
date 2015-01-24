open Graphics

open_graph "300x300"

let nb_digits = 12
let digit_height = 80
let digit_width = 30
let em = 5
let vert_seg_length = (digit_height/2)-em
let hor_seg_length = digit_width -(2*em)
let space = 3*em
let sepColon = 10
let sepSlash=10
let edgeTime = (300-(6*digit_width + 2*sepColon))/2
let edgeDate = (300-(6*digit_width + 2*sepSlash))/2
let sepDateTime = 30
let edgeDown = 40

let horizontal x y (*leftmost point*) = fill_poly [| (x,y); (x+em, y+em); (x+hor_seg_length-em, y+em); (x+hor_seg_length, y); (x+hor_seg_length-em, y-em); (x+em, y-em)|]

let vertical x y (*bottommost point*) = fill_poly [| (x,y); (x-em, y+em); (x-em, y+vert_seg_length-em); (x, y+vert_seg_length); (x+em, y+vert_seg_length-em); (x+em, y+em)|]

let coords i = function
		|5 -> (edgeTime, edgeDown)
		|4 -> (edgeTime + 1*(digit_width +space), edgeDown)
		|3 -> (edgeTime + 2*(digit_width +space) + 1*sepColon, edgeDown)
		|2 -> (edgeTime + 3*(digit_width +space) + 1*sepColon, edgeDown)
		|1 -> (edgeTime + 4*(digit_width +space) + 2*sepColon, edgeDown)
		|0 -> (edgeTime + 5*(digit_width +space) + 2*sepColon, edgeDown)
		|11 -> (edgeDate, edgeDown+digit_height+sepDateTime)
		|10->(edgeDate + 1*(digit_width +space) + 0*sepSlash, edgeDown+digit_height+sepDateTime)
		|9->(edgeDate + 2*(digit_width +space) + 1*sepSlash, edgeDown+digit_height+sepDateTime)
		|8->(edgeDate + 3*(digit_width +space) + 1*sepSlash, edgeDown+digit_height+sepDateTime)
		|7->(edgeDate + 4*(digit_width +space) + 2*sepSlash, edgeDown+digit_height+sepDateTime)
		|_->(edgeDate + 5*(digit_width +space) + 2*sepSlash, edgeDown+digit_height+sepDateTime)
		
		(*returns the coordinates of digit i in the graphical window*)

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
	fill_rect x+((sepColon-4)/2) y+(digit_height/4) 4 4;
	fill_rect x+((sepColon-4)/2) y+(3*(digit_height/4)) 4 4

let display_slash (x,y) = 
	draw_segments [|(x+2, y, x+sepSlash-2, y+digit_height)|]
