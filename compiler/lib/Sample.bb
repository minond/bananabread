def loop(i, fn) =
  if i
  then begin
    fn(i)
    loop(i - 1, fn)
  end
  else fn(0)


loop(10,
  (func (y) =
    func (x) =
      begin
        println(y)
        println(x)
      end
    )(123))
