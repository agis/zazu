-module(ircerl).
-compile(export_all).

start(Host, Port, Nick) ->
  spawn(?MODULE, connect, [Host, Port, Nick]).

connect(Host, Port, Nick) ->
  {ok, Socket} = gen_tcp:connect(Host, Port, [{keepalive, true}]),
  do(Socket, string:join(["NICK", Nick], " ")),
  do(Socket, string:join(["USER", Nick, "0 * :", Nick], " ")),
  do(Socket, "JOIN #hi"), % TODO: remove this l8r
  loop(Socket).

loop(Socket) ->
  receive
    {tcp, _, Msg} ->
      io:format("~p~n", [Msg]),
      handle(Socket, Msg),
      loop(Socket);
    {cmd, quit} ->
      do(Socket, "QUIT"),
      gen_tcp:close(Socket);
    {cmd, Command} ->
      do(Socket, Command),
      loop(Socket)
  end.

% format of each IRC command is :Name COMMAND parameter list
%   ex. :wizy!~test@127.0.0.1 JOIN :#hi\r\n
%
% now parse privmsgs
handle(Socket, Msg) ->
  case string:tokens(Msg, " ") of
    ["PING"|_] -> do(Socket, re:replace(Msg, "PING", "PONG", [{return, list}]));
            _  -> do(Socket, "PRIVMSG #hi :another msg received")
  end.

do(Socket, Command) ->
  case string:right(Command, 2) of
    "\r\n" -> gen_tcp:send(Socket, Command);
    _      -> gen_tcp:send(Socket, string:join([Command, "\r\n"], ""))
  end.

cmd(Pid, Cmd) -> Pid ! {cmd, Cmd}.
