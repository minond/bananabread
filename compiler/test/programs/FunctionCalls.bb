println((let x = 1 in func(y) = x + y)(41))
println((if 1 then func() = "pass" else func() = "fail")())
println((begin func() = "ok" end)())

// Expected output
//
// 42
// pass
// ok
