defmodule GlassCLITest do
  use ExUnit.Case
  doctest GlassCLI

  test "greets the world" do
    assert GlassCLI.hello() == :world
  end
end
