module Sample

import Prelude exposing (-, +, ++, println)

operator(infix, 2, '-)
operator(infix, 2, '+)
operator(infix, 2, '++)

// println("testing")

// def x(n) = n
// def y = x

// println(y("head") ++ "tail")

// def x = 1

def x = 123

println(x + 1)

// %[[
//   println
// ]]

// let x = 1 in x
