defmodule GlassCLI.Client do
    @moduledoc """
        Client module for handling parsed requests
    """
    # request struct
    defstruct(
        query: nil,
        intiated_at: 0,
        results: []
    )

    def start_link(_), do: GenServer.start_link(__MODULE__, [], name: __MODULE__)

    def init(_), do: {:ok, :started}


end