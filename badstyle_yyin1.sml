structure PTR : sig val & : 'a -> 'a ref end = struct val & = fn A => ref A end
structure DREF : sig val ** : (int -> 'a) ref -> int -> 'a end = struct fun ** A i = (!A) i end

(* //@Requires: functions are values *)
(* //@Ensures: things will never go wrong *)
fun is_rotation (A : ''a list) (B : ''a list) : bool = 
    let
        infix 3 <\
        fun x <\ f = fn y => f (x,y)
        infix 7 != 
        fun A != B = not (A = B)
        open PTR
        open DREF
        exception Segmentation_Fault
         exception Core_Dumped
        (* //@ TBH if you want some (really)* bad style just look at my 210 hw *)
        val A' = & (fn x => List.nth (A,x) handle _ => raise Segmentation_Fault)
        val B' = & (fn x => List.nth (B,x) handle _ => raise Segmentation_Fault)
        val factref = & (fn x => 0)
        val (f_u_n_c_t_i_o_n_s_a_r_e__poi_t_e_r_s : bool) = false
        val (functionsarevalues : bool) = true
        val (debug : bool) = functionsarevalues
        val (is_true : bool -> bool) = (fn true => true | false => false)
        val (() : unit) = if (is_true debug) then print "Debug mode is on:\n" 
        else print ""
        val (len_A : int) = List.length A
      val (len_B : int) = List.length B
        val result = if (len_A != len_B) then false else true
        val i = & 0
        val j = & 0
        val flag = & true
        (* you know what, segmentation faults can be easily handled in SML *)
        val A'' = if result != true then false
            else (((while (!j < len_A andalso !flag) do 
            (if ** A' (!j) = ** B' 0 then flag := false else j := !j+1));true) 
                handle Segmentation_Fault => true) (* yes we can! *)
        val () = if is_true debug then print (Int.toString(!j)^"\n") else print ""
          val () = if is_true debug then print ("How powerful!") else print ""
        val () = flag := true
        val result = if result != true then false else if (len_A=0) then true
            else (((while (!i < len_B andalso !flag) do
            (if ** B' (!i) != ** A' (!j) then flag := false 
            else (i:= !i+1;j:=((!j+1) mod len_A))));true) 
                handle Segmentation_Fault => true)
        val () = if (is_true debug) then print "what do you think functions are?\n"
        else print ""
        val result = if (len_A = 0 andalso len_B = 0) then true 
         else if result != true then false else !flag
    in
        if (is_true result) then functionsarevalues else (not true)
    end

