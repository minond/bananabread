module Sample

import Prelude exposing (-, +, ++, println)

operator(infix, 2, '-)
operator(infix, 2, '+)
operator(infix, 2, '++)

println("testing")

def x(n) = n
def y = x

// println(y("head") ++ "tail")
