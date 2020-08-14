defmodule GlassCLI.Parser.Test do
  use ExUnit.Case

  alias GlassCLI.Parser

  test "parse erlang form" do
    query = "write(Value) -> io:fwrite(\"Hello world!~p~n\", [Value])."
    {result, _} = Parser.parse(query)
    assert :ok == result
  end

  test "parse erlang expression" do
    query = "fun(Value) -> io:fwrite(\"Hello world!~p~n\", [Value]) end."
    {result, _} = Parser.parse(query)
    assert :ok == result
  end

  test "empty" do
    {result, _} = Parser.parse("")
    assert :error == result
  end

  test "invalid query" do
    {result, _} = Parser.parse("slfjdalk")
    assert :error == result
  end
end
