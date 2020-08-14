defmodule GlassCLI.Client do
  @moduledoc """
      Client module for handling parsed requests
  """
  use GenServer

  @default_timeout 30_000

  # request struct
  @type state() :: %__MODULE__{
          query: GlassCLI.Query.parsed() | nil,
          results: list(result()),
          uuid: any(),
          finished: boolean(),
          receiver_pid: pid() | nil
        }

  defstruct(
    query: nil,
    results: [],
    uuid: nil,
    finished: true,
    receiver_pid: nil
  )

  # TODO: define result
  @type result() :: any()

  def start_link(_) do
    GenServer.start_link(__MODULE__, [:ok], name: __MODULE__)
  end

  def init(_), do: {:ok, init_state()}

  # interface

  @doc """
  asynchorous call to send the search request
  """
  @spec search(query :: GlassCLI.Query.parsed()) ::
          :ok | {:error, any()}
  def search(query) do
    case GenServer.call(__MODULE__, {:search, query, self()}) do
      :ok ->
        receive do
          {:results, results} ->
            # TODO: pretty print
            results
        after
          @default_timeout ->
            stop()
            {:error, :request_timeout}
        end

      error ->
        error
    end
  end

  @doc """
  function to forcefully stop the current search
  """
  @spec stop() :: :ok
  def stop(), do: GenServer.cast(__MODULE__, :stop)

  @doc """
  function to handle result from backend's rpc call
  """
  @spec result(any(), result()) :: :ok
  def result(uuid, result), do: GenServer.cast(__MODULE__, {:result, {uuid, result}})

  @doc """
  function to mark a search as finished, initiated by backend when a search finishes
  """
  @spec finish(any()) :: :ok
  def finish(uuid), do: GenServer.cast(__MODULE__, {:finish, uuid})

  # handlers

  def handle_call({:search, _query, _pid}, _from, state = %{finished: false}) do
    {:reply, {:error, :search_not_finished}, state}
  end

  def handle_call({:search, query, pid}, _from, state) do
    rpc_timeout = Application.get_env(:glass_cli, :rpc_timeout)
    backend_node = Application.get_env(:glass_cli, :backend_node)
    # TODO: use the correct mod/fun here
    case :rpc.call(
           backend_node,
           :"Elixir.DummyServer",
           :glass_backend,
           [Node.self(), query],
           rpc_timeout
         ) do
      {:ok, uuid} ->
        new_state = %__MODULE__{
          query: query,
          results: [],
          uuid: uuid,
          finished: false,
          receiver_pid: pid
        }

        {:reply, :ok, new_state}

      {:badrpc, reason} ->
        {:reply, {:error, reason}, state}
    end
  end

  def handle_cast(:stop, state), do: {:noreply, %{state | finished: true}}

  def handle_cast({:result, {uuid, result}}, state = %{uuid: uuid}) do
    new_state = %{state | results: [result | state.results]}
    {:noreply, new_state}
  end

  def handle_cast({:result, _}, state), do: {:noreply, state}

  def handle_cast({:finish, uuid}, state = %{uuid: uuid}) do
    send(state.receiver_pid, {:results, state.results})
    {:noreply, %{state | finished: true}}
  end

  # private
  defp init_state(), do: %__MODULE__{finished: true}
end
