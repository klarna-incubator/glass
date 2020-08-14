defmodule GlassCLI do
  @moduledoc """
    Supervisor and interface module for glass_cli
  """

  # supervisor

  use Supervisor
  alias GlassCLI.Parser
  alias GlassCLI.Client

  def start(_type, _args) do
    Supervisor.start_link(__MODULE__, [], name: __MODULE__)
  end

  def init(_) do
    children = [
      {Client, []}
    ]

    Supervisor.init(children, strategy: :one_for_one)
  end

  # interfaces

  @spec search(query :: String.t()) :: [Client.result()] | {:error, any()}
  def search(query) do
    case Parser.parse(query) do
      # TODO: pretty print
      {:ok, parsed} -> Client.search(parsed)
      {:error, error} -> error
    end
  end

  @spec result(any(), any()) :: :ok
  def result(uuid, result), do: Client.result(uuid, result)

  @spec finish(any()) :: :ok
  def finish(uuid), do: Client.finish(uuid)
end
