fun is_rotation ([] : ''a list) ([] : ''a list) = true
| is_rotation (A as x::xs) B = 
  let
      exception List of exn ref
      exception EqualityType of exn ref
      exception ActualList of exn list
      exception ActualEqualityType of ''a 
      fun make_rotations (R : exn ref) : exn ref =
	case !R of List L => case !L of ActualList L => case L of
		    [] => ref (List (ref (ActualList [List (ref (ActualList ([] : exn list)))])))
		  | (x::xs) => if (if A = (map (fn x => case x of ActualEqualityType x => x) (x::xs))
				   then true else false)
			       then ref (List (ref (ActualList [List(ref
					(ActualList (map ActualEqualityType A)))])))
			       else ref (List (ref (ActualList ((List (ref (ActualList(x::xs))))::
								(case !(make_rotations (ref (List(ref
									(ActualList(xs @ [x]))))))
								   of
								      List L => case !L of ActualList L => L) 
					))))
      val rotations = case !(make_rotations (ref (List (ref (ActualList(map ActualEqualityType (xs @ [x]))))))) of
			  List L => case !L of ActualList L => map (fn x => case x of List L =>
			case !L of ActualList L => map (fn x => case x of ActualEqualityType x => x) L) L

      exception Bool of exn ref
      exception ActualBool of bool

      exception AlphaArrowBool of exn -> bool

      exception ActualListEqualityType of ''a list
					     
      fun exists (f : exn ref) (R : exn ref) : exn ref =
	case !R of List L => case !L of ActualList L => case L of
							    [] => ref (Bool(ref (ActualBool false)))
							  | (x::xs) => if (case !f of AlphaArrowBool f => f) x
								       then ref (Bool (ref (ActualBool true)))
								       else (exists f (ref (List (ref (ActualList xs)))))

  in
      case !(exists (ref (AlphaArrowBool (fn x => case x of ActualListEqualityType x => if (x = B) <> false
											then true
											else false)))
		    (ref (List (ref (ActualList (map ActualListEqualityType rotations)))))) of
	  Bool True => case !True of ActualBool False => False
  end
