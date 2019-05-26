defmodule Tparse do
  import NimbleParsec

  defmodule Dev do
    defstruct name: nil, addr: []
  end

  defmodule Defun do
    defstruct fun: nil, arg: nil
  end

  num16 = ignore(string("0x"))
          |> ascii_string([?0..?9, ?a..?f, ?A..?F], min: 1)
          |> map({String, :to_integer, [16]})

  num8 = ignore(string("0o"))
         |> ascii_string([?0..?7], min: 1)
         |> map({String, :to_integer, [8]})

  num2 = ignore(string("0b"))
         |> ascii_string([?0..?1], min: 1)
         |> map({String, :to_integer, [2]})

  num10 = ascii_string([?0..?9], min: 1)
          |> map({String, :to_integer, [10]})

  defcombinatorp :int, choice([num16, num8, num2, num10])


  white_spaces = repeat(ascii_char([?\s, ?\t]))
                 |> replace(:space)

  defcombinatorp :spaces, ignore(white_spaces)


  identifier = ascii_char([?a..?z, ?_])
               |> repeat(ascii_char([?a..?z, ?A..?Z, ?0..?9, ?_]))
               |> reduce({List, :to_string, []})
               |> map({String, :to_atom, []})

  defcombinatorp(:dev_head,
    choice([string("M") |> replace(:M),
      string("X") |> replace(:X), 
      string("Y") |> replace(:Y),
      string("L") |> replace(:L), 
      string("WM") |> replace(:WM),
      string("WL") |> replace(:WL),
      string("T") |> replace(:T)
    ]))

  defcombinatorp(:addr,
    parsec(:int)
    |> repeat(ignore(string(".")) |> parsec(:int))
  )

  dev = parsec(:dev_head)
        |> parsec(:addr)
        |> reduce({:dev_r, []})

  defp dev_r([shm | addrs]) do
    %Dev{name: shm, addr: addrs}
  end

  defcombinatorp(:literal,
    choice([dev, parsec(:int)])
    |> parsec(:spaces)
  )

  defcombinatorp(:function,
    identifier
    |> optional(parsec(:spaces) |> parsec(:expr))
    |> parsec(:spaces)
    |> reduce({:function_r, []})
  )

  defp function_r([funcname | args]) do
    {funcname, args}
  end

  defcombinatorp(:func_def,
    parsec(:function)
    |> ignore(string("="))
    |> parsec(:spaces)
    |> choice([parsec(:statement), parsec(:expr)])
    |> reduce({:func_def_r, []})
  )

  defp func_def_r([fun, expr]) do
    %Defun{fun: fun, arg: expr}
  end

  defcombinatorp(:func_call,
    choice([parsec(:function),
      ignore(string("("))
      |> parsec(:spaces)
      |> parsec(:func_call)
      |> ignore(string(")"))
      |> parsec(:spaces)
    ])
  )

  tuple = ignore(string("(")) 
          |> parsec(:expr)
          |> repeat(ignore(string(",")) |> parsec(:expr))
          |> ignore(string(")"))
          |> reduce({:tuple_r, []})

  defp tuple_r([x | []]) do
    x
  end

  defp tuple_r([x1, x2 | []]) do
    {x1, x2}
  end

  defp tuple_r(xs) do
    {:tuple, xs}
  end

  paren = ignore(string("(")) 
          |> parsec(:expr)
          |> ignore(string(")"))

  list = ignore(string("[")) 
         |> parsec(:expr)
         |> repeat(ignore(string(",")) |> parsec(:expr))
         |> ignore(string("]"))
         |> reduce({:list_r, []})

  defp list_r(xs) do
    xs
  end

  defcombinatorp(:expr, 
    parsec(:spaces)
    |> choice([parsec(:literal), list, paren, parsec(:func_call)])
    |> parsec(:spaces)
  )

  defcombinatorp(:cmd,
    parsec(:spaces)
    |> parsec(:literal)
    |> reduce({:cmd_r, []})
  )
  defp cmd_r([x]) do
    %{cmd: x}
  end

  defcombinatorp(:pair,
    ignore(string("("))
    |> parsec(:expr)
    |> ignore(string(","))
    |> choice([parsec(:cmd), parsec(:statement)])
    |> ignore(string(")"))
    |> parsec(:spaces)
    |> reduce({:pair_r, []})
  )

  defp pair_r([expr, stmt]) do
    {expr, stmt}
  end

  defcombinatorp(:statement, 
    parsec(:spaces)
    |> choice([parsec(:func_def), parsec(:func_call), parsec(:pair)])
  )

  defcombinatorp(:stmts, repeat(parsec(:statement)))

  defcombinatorp(:prog,
    parsec(:spaces)
    |> parsec(:stmts)
  )

  defparsec :prg, parsec(:prog)
end

defmodule Prog do
  def f do
    p(Tparse.prg("(X10, X10)"))
  end

  def p({:ok, [t], _, _, _, _}) do
    t
  end

end
