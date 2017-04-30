(* state monad *)

type ('a,'s) state = 's -> ('a * 's)

fun return a = fn s => (a, s)

infix >>=
fun (st : ('a, 's) state) >>= (f : 'a -> ('b, 's) state) : ('b, 's) state =
  fn s => let val (a, s') = st s in (f a) s' end

infix >>
fun st1 >> st2 = st1 >>= (fn _ => st2)

val get = fn s => (s,s)
fun put s  = fn _ => ((),s)

fun modify f = get >>= (put o f)

fun mapM_ f = foldr (fn (x,y) => f x >> y) (return ())

fun runState st s = st s
fun evalState st s = (fn (a,b) => b) (st s)
fun execState st s = (fn (a,b) => a) (st s)


(* applicative parsec *)


datatype 'a error  = LEFT of string | RIGHT of 'a
type ('a,'s) parser = 's -> ('a error * 's)

(* applicatives are cool *)
infix 0 <$>
fun f <$> p =
  fn s => case p s of
    (LEFT e, res) => (LEFT e, res) | (RIGHT c, res) => (RIGHT (f c), res)

fun pure c = fn s => (RIGHT c, s)

infix 0 <*>
fun ff <*> xx = fn s =>
  case ff s of
    (LEFT e, s')  => (LEFT e, s')
  | (RIGHT f, s') => case xx s' of
                      (LEFT e, s'')  => (LEFT e, s'')
                    | (RIGHT x, s'') => (RIGHT (f x), s'')

infix 0 *>
fun u *> v = pure (fn _ => fn x => x) <*> u <*> v

infix 0 <*
fun u <* v = pure (fn x => fn _ => x) <*> u <*> v

(* back to parsing *)
fun satisfy f = fn s =>
  case s of
    []     => (LEFT "end of stream", [])
  | (c::cs) => if f c then (RIGHT c, cs) else (LEFT "satisfy failed", cs)

fun try p = fn s =>
  case p s of
    (LEFT e, _)   => (LEFT e, s)
  | (RIGHT c, s') => (RIGHT c, s')

infix <|>
fun p1 <|> p2 = fn s =>
  case p1 s of
    (LEFT e, _) => p2 s
  | (RIGHT c, s') => (RIGHT c, s')

fun noneOf s = satisfy (fn c => String.isSubstring (String.str c) s)

fun many p =
let
  fun p' s =
    case p s of
      (LEFT e, _)   => (RIGHT [], s)
    | (RIGHT c, s') => case p' s' of
                         (LEFT e, s'')   => (LEFT e, s'')
                       | (RIGHT cs, s'') => (RIGHT (c::cs), s'')
in
  p'
end

fun ch c = satisfy (fn x => x = c)

(* parsing *)
datatype Insn = MoveOneRight
              | MoveTwoRight
              | MoveOneLeft
              | MoveTwoLeft
              | IncrementOne
              | IncrementTwo
              | DecrementOne
              | DecrementTwo
              | OutputOne
              | OutputTwo
              | XorOne
              | XorTwo
              | ZeroOne
              | ZeroTwo
              | Loop of Insn list


fun gen x y = ch x *> pure y

val parseOneRight = gen #">" MoveOneRight
val parseTwoRight = gen #")" MoveTwoRight
val parseOneLeft  = gen #"<" MoveOneLeft
val parseTwoLeft  = gen #"(" MoveTwoLeft
val parseIncOne   = gen #"+" IncrementOne
val parseIncTwo   = gen #"*" IncrementTwo
val parseDecOne   = gen #"-" DecrementOne
val parseDecTwo   = gen #"/" DecrementTwo
val parseOutOne   = gen #"." OutputOne
val parseOutTwo   = gen #"," OutputTwo
val parseXorOne   = gen #"^" XorOne
val parseXorTwo   = gen #"%" XorTwo
val parseZeroOne  = gen #"z" ZeroOne
val parseZeroTwo  = gen #"Z" ZeroTwo

fun parseLoop s =
let
  val parse' = Loop <$> parseInsns
in
  (ch #"[" *> parse' <* ch #"]") s
end

and parseInsns stream =
let
  val parseInsn =  parseOneRight
               <|> parseTwoRight
               <|> parseOneLeft
               <|> parseTwoLeft
               <|> parseIncOne
               <|> parseIncTwo
               <|> parseDecOne
               <|> parseDecTwo
               <|> parseOutOne
               <|> parseOutTwo
               <|> parseXorOne
               <|> parseXorTwo
               <|> parseLoop
               <|> parseZeroOne
               <|> parseZeroTwo
in
  (many parseInsn) stream
end


(* interpreting *)

type tape = IntInf.int list * IntInf.int * IntInf.int list
type tapes = tape * tape
type runner = (unit, tapes) state


val printHead = print o String.str o Char.chr o IntInf.toInt

fun show ((ls,x,rs),(ls',x',rs')) =
let

  val showL   = map (Int.toString o IntInf.toInt) o List.rev
  fun showH x = ["<"^((Int.toString o IntInf.toInt) x)^">"]
  val showR   = map (Int.toString o IntInf.toInt)

  val t1 = "Tape 1: ["^
    (String.concatWith ", " (showL ls @ showH x @ showR rs))
    ^"]"

  val t2 = "Tape 2: ["^
  (String.concatWith ", " (showL ls' @ showH x' @ showR rs'))
  ^"]"
in
  t1^"\n"^t2^"\n"
end

exception TRUE
exception FALSE

fun runInsn MoveOneLeft = modify
  (fn (([],   x,rs),ts) => (([],0 : IntInf.int,x::rs),ts)
   |  ((l::ls,x,rs),ts) => ((ls,l,x::rs),ts)
  )

  | runInsn MoveTwoLeft = modify
  (fn (os,([],   x,rs)) => (os,([],0 : IntInf.int,x::rs))
   |  (os,(l::ls,x,rs)) => (os,(ls,l,x::rs))
  )

  | runInsn MoveOneRight = modify
  (fn ((ls,x,[]   ),ts) => ((x::ls,0 : IntInf.int,[]),ts)
   |  ((ls,x,r::rs),ts) => ((x::ls,r,rs),ts)
  )

  | runInsn MoveTwoRight = modify
  (fn (os,(ls,x,[]   )) => (os,(x::ls,0 : IntInf.int,[]))
   |  (os,(ls,x,r::rs)) => (os,(x::ls,r,rs))
  )

  | runInsn IncrementOne = modify
  (fn ((ls,x,rs),ts) => ((ls,x+1,rs),ts))

  | runInsn IncrementTwo = modify
  (fn (os,(ls,x,rs)) => (os,(ls,x+1,rs)))

  | runInsn DecrementOne = modify
  (fn ((ls,x,rs),ts) => ((ls,x-1,rs),ts))

  | runInsn DecrementTwo = modify
  (fn (os,(ls,x,rs)) => (os,(ls,x-1,rs)))

  | runInsn OutputOne = raise TRUE

  | runInsn OutputTwo = raise FALSE

  | runInsn XorOne = modify
  (fn ((ls,x,rs),(ls',y,rs')) => ((ls,IntInf.xorb (x,y),rs),(ls',y,rs')))

  | runInsn XorTwo = modify
  (fn ((ls,x,rs),(ls',y,rs')) => ((ls,x,rs),(ls',IntInf.xorb (x,y),rs')))

  | runInsn ZeroOne = modify
  (fn ((ls,x,rs),ts) => ((ls,0 : IntInf.int,rs),ts))

  | runInsn ZeroTwo = modify
  (fn (os,(ls,x,rs)) => (os,(ls,0 : IntInf.int,rs)))

  | runInsn (lp as Loop is) =
  get >>= (fn ((_,x,_), (_,y,_)) => if x <> y then return () else runInsns is >> runInsn lp)


and runInsns xs = mapM_ runInsn xs

fun main s =
  case (parseInsns o String.explode) s of
    (LEFT e, _)   => (print e)
  | (RIGHT is, _) =>
      let
        val initialState = (([],5 : IntInf.int,[]),([],5 : IntInf.int,[]))
        val st = runInsns is
        val endState = evalState st initialState
      in
        print (show endState)
      end

val checkTrue =
  "(((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((([.]))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))"

val checkFalse =
  "<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<[,]>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>"

val code = "<([/)>[)>]"^checkTrue^"<([<(]Z)Z)"^checkFalse^"(]"

fun is_rotation [] [] = true
  | is_rotation l1 l2 = if length l1 <> length l2 then false else
let
  val x1::rs1 = map (IntInf.fromInt o (fn x => x + 0)) l1
  val x2::rs2 = map (IntInf.fromInt o (fn x => x + 0)) l2
  val initialState = (([],x1,rs1),([],x2,rs2@(x2::rs2)))
in
  case (parseInsns o String.explode) code of
    (LEFT e, _)   => (print e; false)
  | (RIGHT is, _) =>
      (let
        val endState = evalState (runInsns is) initialState
        val _ = print (show endState)
      in
        false
       end) handle TRUE => true | FALSE => false
end
