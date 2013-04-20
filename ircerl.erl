-module(ircerl).
-compile(export_all).

start() ->
  spawn(?MODULE, connect, ["127.0.0.1", 6667]).

connect(Host, Port) ->
  {ok, Socket} = gen_tcp:connect(Host, Port, [{keepalive, true}]),
  register(Socket),
  loop(Socket).

register(Socket) ->
  do(Socket, "NICK y0bot\r\nUSER yobot 0 * :justabot\r\n"),
  do(Socket, "JOIN #hi\r\n").

loop(Socket) ->
  receive
    {tcp, _, X} ->
      io:format("~p~n", [X]),
      do(Socket, "PRIVMSG #hi :received!\r\n"),
      loop(Socket);
    {cmd, quit} ->
      do(Socket, "QUIT\r\n"),
      gen_tcp:close(Socket)
  end.

do(Socket, Command) ->
  gen_tcp:send(Socket, Command).
