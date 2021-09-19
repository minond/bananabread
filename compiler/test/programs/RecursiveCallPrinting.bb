def loop(i : I32, fn : I32 -> Void) : Void =
  if i
  then begin
    fn(i)
    loop(i - 1, fn)
  end
  else fn(0)


loop(10,
  (func (y : I32) : I32 -> Void =
    func (x : I32) : Void =
      begin
        println(y)
        println(x)
      end
    )(123))

// Expected output
//
// 123
// 10
// 123
// 9
// 123
// 8
// 123
// 7
// 123
// 6
// 123
// 5
// 123
// 4
// 123
// 3
// 123
// 2
// 123
// 1
// 123
// 0
