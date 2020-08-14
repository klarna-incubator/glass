# GlassCLI

## TODO List:
* create shell script for calling search query from terminal
* pretty print results
* configure node/function/args to align with glass_backend

## Test run with DummyServer:
1. start glass_cli `iex --sname foo -S mix`
2. start dummy_server `iex --sname bar -S mix`
3. set cookie on glass_cli `:erlang.set_cookie(bar@localhost, {--dummy_s_cookie--})`
4. run search
```
iex(foo@localhost)23> query = "write(Value) -> io:fwrite(\"Hello world!~p~n\", [Value])."
"write(Value) -> io:fwrite(\"Hello world!~p~n\", [Value])."
iex(foo@localhost)24> GlassCLI.search(query)
```
5. mock return on dummy_server
```
  def send_result() do
    :rpc.call(:foo@localhost, :"Elixir.GlassCLI", :result, [1, :result1])
    :timer.sleep(3_000)
    :rpc.call(:foo@localhost, :"Elixir.GlassCLI", :result, [1, :result2])
    :timer.sleep(4_000)
    :rpc.call(:foo@localhost, :"Elixir.GlassCLI", :result, [1, :result3])
    :timer.sleep(5_000)
    :rpc.call(:foo@localhost, :"Elixir.GlassCLI", :finish, [1])
    :rpc.call(:foo@localhost, :"Elixir.GlassCLI", :result, [1, :result4])
  end
```
```
iex(bar@localhost)10> DummyServer.send_result
:ok
```
6. after receiving :finish message
```
iex(foo@localhost)24> GlassCLI.search(query)
[:result3, :result2, :result1]
```
