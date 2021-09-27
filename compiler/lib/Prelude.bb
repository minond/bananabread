module Prelude


operator(infix, 2, '++)
operator(infix, 2, '+)
operator(infix, 2, '-)

def println[T](x : T) : T =
  %[[
    load [Str] x
    println
    load [Str] x
  ]]

def +(a : I32, b : I32) : I32 =
  %[[
    load [I32] a
    load [I32] b
    add [I32]
  ]]

def -(a : I32, b : I32) : I32 =
  %[[
    load [I32] a
    load [I32] b
    sub [I32]
  ]]

def ++(a : Str, b : Str) : Str =
  %[[
    load [Str] a
    load [Str] b
    concat
  ]]
