# Bad Style Contest
The 15-150 (Spring 2017) TAs have been making submission to a bad style contest. Below we have submissions from 6 TAs, all given the following problem:

    Write the function is_rotation, which takes in two values of type ''a list, and determines if one of the lists is a rotation of the other. 

    That is, if the lists have the same values in the same order, possibly with the indices of their elements shifted by some number. For example, 

    is_rotation [1, 2, 3, 4, 5] [4, 5, 1, 2, 3] ==> true 
Recall that ''a denotes any type that can be compared using =. 

A regular solution to this problem might look something like this:

```
fun is_rotation (A : ''a list) (B : ''a list) =
  let
      fun check_n [] [] 0 = true
        | check_n A (B as x::xs) i =
          i <> 0 andalso (A = B orelse (check_n A (xs @ [x]) (i - 1)))
  in
      length A = length B andalso check_n A B (length A)
  end
```
And so, we present to you, all of the worst ways that TAs have found to write this simple function.

## 1. Lizzi

This solution makes great use of all the features of SML that we don't talk about - including refs and while loops. It even uses some *gasp* C syntax for extra horrific style.

[Lizzi's Solution](https://github.com/jluningp/tabadstyle/blob/master/badstyle_yyin1.sml)

## 2. Vijay

This solution uses some very well named variables, including a function called ?, to spell out some interesting English sentences in code.

[Vijay's Solution](https://github.com/jluningp/tabadstyle/blob/master/badstyle_vijayram.sml)

## 3. Sushain

This solution makes great use of randomness - in fact, it uses randomness so well that even though the code should theoretically terminate, 
it timed out on Autolab before running even one test case. It also features some wonderful test-driven development.

[Sushain's Solution](https://github.com/jluningp/tabadstyle/blob/master/badstyle_scherivi.sml)

## 4. Richard

This solution is  a work of art - quite literally. It spells out a lambda in code. 

[Richard's Solution](https://github.com/jluningp/tabadstyle/blob/master/badstyle_rmfan.sml)

## 5. Jeanne

This solution makes good use of the exn type. In fact, it's so good at using the exn type that all helper functions have type exn ref -> exn ref. 

[Jeanne's Solution](https://github.com/jluningp/tabadstyle/blob/master/badstyle_jluningp.sml)

## 6. Chris

This solution implements a two tape version of the esoteric language Brainfuck, and then solves the problem in the language. Not only does it 
use a different language, it also assumes that lists are less than 500 elements long and claims ''a = int. 

[Chris's Solution](https://github.com/jluningp/tabadstyle/blob/master/badstyle_cgrossac.sml)
