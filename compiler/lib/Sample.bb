module Sample

import Prelude

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
