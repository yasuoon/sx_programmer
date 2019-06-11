defmodule Token do
  import NimbleParsec

  # Integer
  
  defmodule Int do
    defstruct num: 0, base: 10
  end

  num16 = ignore(string("0x"))
          |> ascii_string([?0..?9, ?a..?f, ?A..?F], min: 1)
          |> map({:num_r, [16]})

  num8 = ignore(string("0o"))
         |> ascii_string([?0..?7], min: 1)
         |> map({:num_r, [8]})

  num2 = ignore(string("0b"))
         |> ascii_string([?0..?1], min: 1)
         |> map({:num_r, [2]})

  num10 = ascii_string([?0..?9], min: 1)
          |> map({:num_r, [10]})

  def num_r(num, base) do
    %Int{num: String.to_integer(num, base), base: base}
  end

  defcombinatorp(:integer,
    choice([num16, num8, num2, num10])
  )

  # White Spaces and New Line and Comment

  defcombinatorp(:spaces,
    parsec(:white_space)
    |> repeat()
    |> ignore()
  )

  defcombinatorp(:white_space,
    times(parsec(:white_char), min: 1)
  )

  defcombinatorp(:white_char,
    choice([ascii_char([?\s, ?\t]), parsec(:newline), parsec(:comment)])
  )

  defcombinatorp(:newline,
    choice([string("\r\n"), string("\r"), string("\n")])
    |> replace(:newline)
  )

  defcombinatorp(:comment,
    string("#")
    |> repeat(utf8_char(not: ?\n, not: ?\r))
    |> replace(:comment)
  )

  # SX Programmer Device

  defmodule Dev do
    defstruct type: nil, addr: []
  end

  defcombinatorp(:dev_type,
    choice([string("M") |> replace(:M),
      string("X") |> replace(:X), 
      string("Y") |> replace(:Y),
      string("L") |> replace(:L), 
      string("WM") |> replace(:WM),
      string("WL") |> replace(:WL),
      string("T") |> replace(:T)
    ]))

  defcombinatorp(:dev_addr,
    parsec(:integer)
    |> repeat(ignore(string(".")) |> parsec(:integer))
  )

  dev = parsec(:dev_type)
        |> parsec(:dev_addr)
        |> reduce({:dev_r, []})

  defp dev_r([t | addrs]) do
    %Dev{type: t, addr: addrs}
  end

  # Identifier

  defcombinatorp(:small, ascii_char([?a..?z, ?_]))

  defcombinatorp(:large, ascii_char([?A..?Z]))

  defcombinatorp(:digit, ascii_char([?0..?9]))

  defcombinatorp(:id_body,
    choice([parsec(:small), parsec(:large), parsec(:digit)])
  )

  defmodule VarId do
    defstruct id: nil
  end

  defcombinatorp(:var_id,
    parsec(:small)
    |> repeat(parsec(:id_body))
    |> parsec(:spaces)
    |> reduce({:var_id_r, []})
  )

  defp var_id_r(name) do
    %VarId{id: List.to_atom(name)}
  end

  # Literal
  
  defcombinatorp(:literal,
    choice([dev, parsec(:integer)])
    |> parsec(:spaces)
  )

  # Special
  
  defcombinatorp(:special,
    ascii_char([?;, ?[, ?], ?`, ?{, ?}])
    |> parsec(:spaces)
    |> reduce({List, :to_atom, []})
  )

  defcombinatorp(:comma,
    string(",")
    |> parsec(:spaces)
    |> ignore()
  )
  
  # Paren
  
  defcombinatorp(:lparen,
    string("(")
    |> parsec(:spaces)
    |> ignore()
  )

  defcombinatorp(:rparen,
    string(")")
    |> parsec(:spaces)
    |> ignore()
  )

  defcombinatorp(:paren,
    parsec(:lparen)
    |> parsec(:expr)
    |> repeat(parsec(:comma) |> parsec(:expr))
    |> parsec(:rparen)
    |> reduce({:paren_r, []})
  )

  defp paren_r([x]) do
    x
  end

  defp paren_r(x) do
    {:tuple, x}
  end
  
  # List

  #defcombinatorp(:list,
  #  ignore(string("["))
  #  |> parsec(:lexemes)
  #  |> repeat(ignore(string(",")) |> parsec(:lexemes))
  #  |> ignore(string("]"))
  #  |> reduce({:list_r, []})
  #)


  #defp list_r(x) do
  #  x
  #end
  
  # Operator

  defmodule Ope do
    defstruct op: nil
  end

  defcombinatorp(:operator,
    choice([string("=="),
      string("<="),
      string(">="),
      string("="),
      string("<"),
      string(">"),
      string("+"),
      string("-"),
      string("*"),
      string("/"),
      string("&"),
      string("|")
    ])
    |> parsec(:spaces)
    |> map({:operator_r, []})
  )

  defp operator_r(op) do
    %Ope{op: op}
  end

  #defcombinatorp(:binary_ope,
  #  parsec(:lexeme)
  #  |> ignore(parsec(:spaces))
  #  |> parsec(:operator)
  #  |> ignore(parsec(:spaces))
  #  |> parsec(:lexeme)
  #  |> reduce({:binary_ope_r, []})
  #)

  #defp binary_ope_r([a, op, b]) do
  #  {op, [a, b]}
  #end


  # Function
  #

  defmodule Function do
    defstruct name: nil, args: []
  end

  defcombinatorp(:factor,
    choice([parsec(:paren),
      parsec(:literal),
      parsec(:var_id),
    ])
  )

  defcombinatorp(:args,
    parsec(:factor)
  )

  defcombinatorp(:function,
    parsec(:var_id)
    |> repeat(parsec(:args))
    |> reduce({:function_r, []})
  )

  defp function_r([name]) do
    %Function{name: name}
  end

  defp function_r([name | args]) do
    %Function{name: name, args: args}
  end

  # Expr
  #


  defcombinatorp(:term,
    choice([parsec(:function),
      parsec(:factor),
      parsec(:special),
      parsec(:operator),
    ])
  )

  defcombinatorp(:expr,
    parsec(:spaces)
    |> repeat(parsec(:term))
  )

  # Progmam Parser

  defcombinatorp(:program, parsec(:expr))

  defparsec :prog, parsec(:program)

end



defmodule  Hparse do
  @txt """
  a = 3
  (X1.0, (M1, out Y1.2))
  (M10 & M11, out Y1.3)
  """

  defmodule Function do
    defstruct name: nil, args: []
  end

  def xx do
    Token.prog(@txt)
  end

  def hparse(str) do
    tks = Token.prog(str)
          |> put_tokens
    expr({tks, []})
  end

  def put_tokens({:ok, tk, _, _, _, _}) do
    tk
  end

  def tok({[t | _], _}) do
    t
  end

  def push({tokens, stack}, x) do
    {tokens, [x | stack]}
  end

  def pop({_, [s | ss]}, x) do
    s
  end

  def next({[t | ts], stack}) do
    {ts, stack}
  end

  def ignore(p, x) do
    if tok(p) == x do
      next(p)
    else
      {:error}
    end
  end

  def expr(p) do
    case tok(p) do
      :"(" -> paren(p)
      %Token.VarId{} -> function(p)
    end
  end

  def function(p) do
    in_func(next(p), %Function{name: tok(p), args: []})
  end

  def in_func(p, x) do
    :undefined
  end

  def paren(p) do
    ignore(p, :"(")
    |> in_paren([])
  end

  def in_paren(p, r) do
    case tok(p) do
      :")" -> next(p) |> push(r)
      :"," -> next(p) |> taple([r])
      _  -> next(p) |> in_paren([tok(p) | r])
    end
  end

  def taple(p, r) do
    case tok(p) do
      :")" -> next(p) |> push({:tuple, r})
      :"," -> next(p) |> taple([r])
      _  -> next(p) |> taple([tok(p) | r])
    end
  end

end
