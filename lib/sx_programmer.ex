defmodule SxProgrammer do
  @moduledoc """
  Documentation for SxProgrammer.
  """

  defmodule Dev do
    defstruct [:kind, :index]
  end

  defmodule Bit do
    defstruct [:dev, :op]
  end
  alias NimbleCSV.RFC4180, as: CSV

  @head [0, 255, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,]

  @dest "temp/test001/test001/Resource/test001.il"

  #@doc false
  def hello do
    CSV.dump_to_iodata [@head]
  end

  def testb1(fi, fo, ft, count) do
    Enum.map(0..(count - 1), fn i ->
      od = fo + i
      id = fi + i
      td = ft + i 
      [ ["LD", "M#{id}"],
        ["AND", "T#{td-1}"],
        ["OR", "M#{od}"],
        ["AND", "M#{od - 1}"],
        ["LD", "M199"],
        ["ORI", "M199"],
        ["ANB"],
        ["LD", "M199"],
        ["AND", "M199"],
        ["ORB"],
        ["MPS"],
        ["ANI", "M199"],
        ["OUT", "M#{od}"],
        ["MRD"],
        ["AND", "M199"],
        ["OUT", "M#{od + 200}"],
        ["MRD"],
        ["AND", "M199"],
        ["OUT", "M#{od + 400}"],
        ["MPP"],
        ["TON", "T#{td}", "1000ms"],
        ["TON", "T#{td+100}", "DL#{fo+i*2}"]
      ]
    end)
    |> mk_list()
  end

  def setrwbs do
    w = [[["LD", "ON"], ["MC", "0", "M651004"]]] ++
      ((for n <- 1..5, do: setw1(n))) ++
        [[["MCR", "0"]]]
    r = [[["LD", "ON"], ["MC", "0", "M651005"]]] ++
      ((for n <- 1..5, do: setr1(n))) ++
        [[["MCR", "0"]]]
    mk_list(w ++ r)
    |> write
  end

  def setw1(n) do
    exp = [["LD", "M1002F"],
      ["AND", "M10010"],
      ["AND=", "#{n}", "WL920"]
    ]
    cmds = for i <- 0..9 do
      ["MUL", "DL#{820+i*2}", "1000", "DL#{100+(n-1)*20+i*2}"]
    end
    exp ++ cmds
  end

  def setr1(n) do
    exp = [["LD+", "M10010"],
      ["OR+", "M10011"],
      ["OR+", "M10012"],
      ["AND=", "#{n}", "WL920"]
    ]
    cmds = for i <- 0..9 do
      ["DIV", "DL#{100+(n-1)*20+i*2}", "1000", "DL#{820+i*2}"]
    end
    exp ++ cmds
  end

  def mk_list(ls) do
    Enum.with_index(ls)
    |> Enum.map(fn {block, i} ->
      mk_block(block, i)
    end)
    |> Enum.concat
  end

  def mk_block(ls, count) do 
    Enum.with_index(ls)
    |> Enum.map(fn {row, i} ->
      mk_row(row, count, i)
    end)
  end

  def mk_row([command | args], bc, rc) do
    [bc, rc, command, Enum.count(args) | args]
  end

  def iodata(b) do
    #d = blocks1(100, 500, 20, 10)
    Enum.concat([@head], b)
    |> CSV.dump_to_iodata
  end

  def write(b) do
    File.write(@dest, iodata(b))
  end
end
