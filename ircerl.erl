-module(ircerl).
-compile(export_all).

start(Host, Port, Nick) ->
  spawn(?MODULE, connect, [Host, Port, Nick]).

connect(Host, Port, Nick) ->
  {ok, Socket} = gen_tcp:connect(Host, Port, [{keepalive, true}]),
  do(Socket, string:join(["NICK", Nick, "\r\n"], " ")),
  do(Socket, "USER justabot 0 * :justabot\r\n"),
  do(Socket, "JOIN #hi\r\n"),
  loop(Socket).

loop(Socket) ->
  receive
    {tcp, _, "PING :irc.exampl2e.net\r\n"} ->
      io:format("~p~n", ["ping received"]),
      do(Socket, "PONG yothere irc.exampl2e.net"),
      loop(Socket);
    {tcp, _, X} ->
      io:format("~p~n", [X]),
      do(Socket, "PRIVMSG #hi :received!"),
      loop(Socket);
    {cmd, quit} ->
      do(Socket, "QUIT"),
      gen_tcp:close(Socket);
    {cmd, Command} ->
      do(Socket, Command),
      loop(Socket)
  end.

do(Socket, Command) ->
  gen_tcp:send(Socket, string:join([Command, "\r\n"], "")).

% cmd(P, Cmd) -> P ! {cmd, Cmd}.
