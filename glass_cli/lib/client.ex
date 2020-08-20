defmodule GlassCLI.Client do
  @moduledoc """
      Client module for handling parsed requests
  """
  use GenServer

  @default_timeout 30_000

  # request struct
  @type state() :: %__MODULE__{
          results: list(result()),
          uuid: any(),
          finished: boolean(),
          receiver_pid: pid() | nil
        }

  defstruct(
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
  function to initialise indexing from backend's rpc call
  """
  @spec index(workspace :: String.t(), path :: String.t()) :: {:ok, :indexed}
  def index(workspace, path) do
    sync_call(:index, {workspace, path})
  end

  @doc """
  asynchorous call to send the search request
  """
  @spec search(workspace :: String.t(), query :: GlassCLI.Query.parsed()) ::
          :ok | {:error, any()}
  def search(workspace, query) do
    sync_call(:search, {workspace, query})
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

  @doc """
  function to mark an indexing as finished, initiated by backend when a search finishes
  """
  @spec finish_index() :: :ok
  def finish_index(), do: GenServer.cast(__MODULE__, {:finish, :index})

  # handlers

  def handle_call({:search, _query, _pid}, _from, state = %{finished: false}) do
    {:reply, {:error, :search_not_finished}, state}
  end

  def handle_call({:search, {workspace, query}, pid}, _from, state) do
    do_rpc(:glass, :search, [workspace, query, Node.self()], pid, state)
  end

  def handle_call({:index, {workspace, path}, pid}, _from, state) do
    do_rpc(:glass, :index, [workspace, path, Node.self()], pid, state)
  end

  def handle_cast(:stop, state), do: {:noreply, %{state | finished: true}}

  def handle_cast({:result, {uuid, result}}, state = %{uuid: uuid}) do
    new_state = %{state | results: [result | state.results]}
    {:noreply, new_state}
  end

  def handle_cast({:result, _}, state), do: {:noreply, state}
  
  def handle_cast({:finish, uuid}, state = %{uuid: uuid}), do: do_finish({:search, state.results}, state)
  def handle_cast({:finish, :index}, state), do: do_finish({:index, :indexed}, state)
  def handle_cast(_, state), do: {:noreply, state}

  # private
  defp init_state(), do: %__MODULE__{finished: true}

  defp sync_call(op, msg) do
    case GenServer.call(__MODULE__, {op, msg, self()}) do
      :ok ->
        receive do
          {:search, results} ->
            results
          {:index, :indexed} ->
            {:ok, :indexed}
        after
          @default_timeout ->
            stop()
            {:error, :request_timeout}
        end

      error ->
        error
    end
  end

  defp do_finish(msg, state) do
    send(state.receiver_pid, msg)
    {:noreply, %{state | finished: true}}
  end

  defp do_rpc(mod, fun, args, pid, state) do
    rpc_timeout = Application.get_env(:glass_cli, :rpc_timeout)
    backend_node = Application.get_env(:glass_cli, :backend_node)
    # TODO: use the correct mod/fun here
    case :rpc.call(backend_node, mod, fun, convert_to_list(args), rpc_timeout) do
      :ok ->
        new_state = %__MODULE__{
          results: [],
          uuid: 0,
          finished: false,
          receiver_pid: pid
        }

        {:reply, :ok, new_state}
      {:ok, uuid} ->
        new_state = %__MODULE__{
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
end
