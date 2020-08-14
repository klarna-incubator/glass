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
      {ClassCLI.Client, []}
    ]
    Supervisor.init(children, strategy: :one_for_one)
  end

  # interfaces

  @spec search(query :: String.t()) :: :ok | {:error, error :: atom()}
  def search(query) do
    case Parser.parse(query) do
      {:ok, parsed} -> Glient.search(query)
      {:error, error} -> error
    end
end
