defmodule SxProgrammerTest do
  use ExUnit.Case
  doctest SxProgrammer

  test "greets the world" do
    assert SxProgrammer.hello() == :world
  end
end
