println((let x = 1 in func(y) = x + y)(41))
println((if 1 then func() = "pass" else func() = "fail")())
println((if 0 then func() = "fail" else func() = "pass")())
println((begin func() = "ok" end)())

// Expected output
//
// 42
// pass
// pass
// ok
