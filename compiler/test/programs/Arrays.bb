def genarray() = [9, 8, 7, 6]
def arr1 = [1, 2, 777, 4]
def arr2 = genarray()

println(arr1(0))
println(arr1(1))
println(arr1(2))
println(arr1(3))
println(arr2(0))
println(arr2(1))
println(arr2(2))
println(arr2(3))
println([1, 2, 3, 4](0))
println([1, 2, 3, 4](1))
println([1, 2, 3, 4](2))
println([1, 2, 3, 4](3))

// Expected output
//
// 1
// 2
// 777
// 4
// 9
// 8
// 7
// 6
// 1
// 2
// 3
// 4
