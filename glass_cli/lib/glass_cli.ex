defmodule GlassCLI do
  @moduledoc """
    Supervisor and interface module for glass_cli
  """

  # supervisor

  use Supervisor
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

  @spec index(workspace :: String.t(), path :: String.t()) :: :ok | {:error, any()}
  def index(workspace, path), do: Client.index(workspace, path)

  @spec finish_index() :: :ok | {:error, any()}
  def finish_index(), do: Client.finish_index()

  @spec search(workspace :: String.t(),
               query :: String.t()) :: [Client.result()] | {:error, any()}
  def search(workspace, query) do
    Client.search(workspace, query)
  end

  @spec result(any(), any()) :: :ok
  def result(uuid, result), do: Client.result(uuid, result)

  @spec finish(any()) :: :ok
  def finish(uuid), do: Client.finish(uuid)
end
