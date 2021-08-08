operator(prefix, 0, opcode)
operator(infix, 2, +)
operator(infix, 2, -)

def println[T](x: T): Str =
  opcode %{
    load [Str] x
    println
    ret
  }

def +(a : I32, b : I32) : I32 =
  opcode %{
    load [I32] a
    load [I32] b
    add [I32]
  }

def -(a : I32, b : I32) : I32 =
  opcode %{
    load [I32] a
    load [I32] b
    sub [I32]
  }

def loop(i, fn) =
  if i
  then begin
    fn(i)
    loop(i - 1, fn)
  end
  else fn(0)


loop(1000, println)
