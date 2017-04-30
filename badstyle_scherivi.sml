local
	structure Meow = 
	 	struct
	 		open List;
			open UnixSignals

			val source_of_life = Random.rand (412, 97);
			fun throw_in_air l =
				let
					fun boooooooo (y::xs, 0) = (y, xs)
					|	boooooooo (x::xs, i) = ((#1 (boooooooo (xs, i - 1))), x::(#2 (boooooooo (xs, i - 1))))
					fun quack [] _ = []
					|	quack ys lots =
							let 
								val stuff = boooooooo (ys, Random.randRange (0, lots - 1) source_of_life);
								(*val debug = print "bleh?"*)
							in 
								(#1 stuff)::(quack (#2 stuff) (lots - 1))
							end
				in
					quack l (List.length l)
				end

			fun insecure_eq x = 
				fn y => (* this is how currying works right *)
					(x = [] andalso y = [] andalso List.length x = 0) orelse (
						(List.length x                    = List.length y) andalso
						(List.nth (x, 0) = List.nth (y, 0)) andalso
						(List.hd x                        = List.hd y) andalso (* ha! you can't trick me *)
						(
							String.explode (ListFormat.listToString (fn _ => "???") x) = (* resists all exploits *)
							String.explode (ListFormat.listToString (fn _ => "???") y)
						) andalso
						(insecure_eq (List.tl x) (List.tl y))
					)
			fun sequre_eq x y = (* equality is really tricky *)
				let
					val maybe_equal = insecure_eq x y
					fun definitely_equal () = 
						let
							val definitely_equal = ref false
							val try_this_much_at_least = Random.randInt source_of_life
							val try_this_much_at_least = ref (if try_this_much_at_least > 0 then try_this_much_at_least else ~try_this_much_at_least)
						in
							while ((not (!definitely_equal) andalso maybe_equal) orelse (!try_this_much_at_least > 0 andalso maybe_equal)) do (
								print "hi!! ";
								try_this_much_at_least := (!try_this_much_at_least) - 1;
								definitely_equal := (insecure_eq 
									(throw_in_air x) 
									(throw_in_air y))
							); (!definitely_equal)
						end
				in
					definitely_equal ()
				end
			val really_sequre_eq = fn x => 
				fn y => (sequre_eq x y) andalso (sequre_eq (rev x) (rev y)) (* multiplicity of equality or something**)

			fun for (i : 'a, t : 'a -> bool, u : 'a -> 'a) (f : 'a -> 'b) = (* while syntax is too messy ... *)
				let
					val v = ref i
				in
					while t (!v) do (
						let 
							val r = f (!v)
						in
							(v := (u (!v)); r)
						end
					)
				end

			fun the_best_append x y = 
				let
					exception SecretSause of int
					exception FINISHED_EARLY
					val l = length (* length is too long - this saves developer time *)
					val result = tabulate (l x + l y, fn _ => ref NONE)
					val complete = List.all (Option.isSome o !) (* #pointsomethinguhuhhhfunctionsarevalues *)
					val try_this_much_at_least = Random.randInt source_of_life (* you know how this goes *)
					val try_this_much_at_least = if try_this_much_at_least > 0 then try_this_much_at_least else ~try_this_much_at_least
					val hopefully_done = (* O(1) - TODO: speed up *) (* the underscore means unused later #style *)
						for (0, fn i => i < try_this_much_at_least, fn i => i + 1) ( (* parallelized and concurrent *)
							fn i => 
								(
									if complete result then raise FINISHED_EARLY (* continuation passing style I think *)
									else raise SecretSause i
								) handle SecretSause n => ( (* big O of zero!! *)
									let
										val pointer = nth (result, i) (* atomic and thread safe *)
									in
										pointer := SOME (if i < l x then nth (x, i) else nth (y, i - (l x)))
									end
								)
						) handle FINISHED_EARLY => ()
				in
					if complete result then (map (Option.valOf o !) result)
					else x @ y before print "WHAT IS GOING WRONG???? Treading carefully." (* really bad - we don't want to get here *)
				end

			infix 1 >>= flat_map bind
			fun (l : 'a list) flat_map (f : 'a -> 'b list) : 'b list = List.concat (List.map f l)
			val (op bind, op >>=) = (op flat_map, op flat_map)

			fun no_discrimination (_, 0) = [[]]
			|	no_discrimination (l, r) =
					let
						fun pick [] = []
						|	pick (x::xs) = (x, xs) :: (map (fn (x', xs) => (x', x::xs)) (pick xs))
					in
						(pick l) flat_map (fn (x, l') => map (fn l'' => x::l'') (no_discrimination (l', r - 1)))
					end

			fun literally_everything x = tabulate (length x, fn n => the_best_append (drop (x, n)) (take (x, n)))

			fun is_rotation A B = 
				let val a = no_discrimination (A, length A) val b = literally_everything A in foldl (fn (x, y) => x orelse y) false (map (fn y => (really_sequre_eq y B) andalso exists (fn k => really_sequre_eq k y) b) a) end

			fun test_driven_development_is_rotation [] [] = true
			|	test_driven_development_is_rotation A B = is_rotation A B
	 	end
 	in
 		val is_rotation = Meow.test_driven_development_is_rotation;
	end

val true = is_rotation [1, 2, 3, 4, 5] [4, 5, 1, 2, 3]
val true = is_rotation [1, 2, 3, 4, 5] [1, 2, 3, 4, 5]
val false = is_rotation [1, 2, 3, 4, 5] [4, 5, 1, 2]
val false = is_rotation [1, 2, 3, 4, 5] [4, 5, 3, 2, 1]
val false = is_rotation [1, 2, 3, 4, 5] []
val true = is_rotation [] []