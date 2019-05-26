defmodule Hparse do
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
    repeat(parsec(:white_space))
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

  # Itentifier

  defcombinatorp(:small, ascii_char([?a..?z, ?_]))

  defcombinatorp(:large, ascii_char([?A..?Z]))

  defcombinatorp(:digit, ascii_char([?0..?9]))

  defcombinatorp(:id_body,
    choice([parsec(:small), parsec(:large), parsec(:digit)])
  )

  defmodule VarId do
    defstruct name: nil
  end

  defcombinatorp(:var_id,
    parsec(:small)
    |> repeat(parsec(:id_body))
    |> reduce({:var_id_r, []})
  )

  defp var_id_r(name) do
    %VarId{name: List.to_atom(name)}
  end

  # Literal
  
  defcombinatorp(:literal,
    choice([dev, parsec(:integer)])
  )

  # Special
  
  defcombinatorp(:special,
    ascii_char([?(, ?), ?,, ?;, ?[, ?], ?`, ?{, ?}])
    |> reduce({List, :to_atom, []})
  )
  
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
    |> map({:operator_r, []})
  )

  defp operator_r(op) do
    %Ope{op: op}
  end

  # Lexeme

  defcombinatorp(:lexeme,
    choice([parsec(:var_id),
      parsec(:literal),
      parsec(:special),
      parsec(:operator)
    ])
  )

  # Progmam Parser

  defcombinatorp(:program,
    repeat(
      parsec(:lexeme)
      |> ignore(parsec(:spaces))
    )
  )

  defparsec :prog, parsec(:program)

end
