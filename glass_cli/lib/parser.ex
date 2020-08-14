defmodule GlassCLI.Parser do
  @moduledoc """
      Module for parsing incoming requests
  """

  @spec parse(query :: String.t()) :: :ok | {:error, atom()}
  def parse(query) do
    {:ok, tokens, _} = :erl_scan.string(String.to_charlist(query))
    # erl_parse supports different functions for parsing expression
    # or forms.
    # For differences between form and expression see:
    # https://erlang.org/doc/apps/erts/absform.html
    case :erl_parse.parse_exprs(tokens) do
      {:ok, ast} ->
        {:ok, ast}

      {:error, _} ->
        :erl_parse.parse_form(tokens)
    end
  end
end
