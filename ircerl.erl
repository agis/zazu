-module(ircerl).
-export([start/3, cmd/2]).

start(Host, Port, Nick) ->
  spawn(fun() -> connect(Host, Port, Nick) end).

connect(Host, Port, Nick) ->
  {ok, Socket} = gen_tcp:connect(Host, Port, [{keepalive, true}]),
  do(Socket, string:join(["NICK", Nick], " ")),
  do(Socket, string:join(["USER", Nick, "0 * :erni3 bot"], " ")),
  loop(Socket).

loop(Socket) ->
  receive
    {tcp, _, Msg} ->
      io:format("~p~n", [Msg]),
      handle(Socket, Msg),
      loop(Socket);
    {cmd, quit} -> % maybe we don't want this
      do(Socket, "QUIT"),
      gen_tcp:close(Socket);
    {cmd, Command} ->
      do(Socket, Command),
      loop(Socket)
  end.

handle(Socket, Msg) ->
  case string:tokens(Msg, " ") of
    ["PING"|_] ->
      do(Socket, re:replace(Msg, "PING", "PONG", [{return, list}]));
    [User, "PRIVMSG", Channel|Message] ->
      handle(Socket, User, Channel, Message);
    _ ->
      loop(Socket)
  end.

handle(Socket, User, Channel, Message) ->
  Nick = string:sub_string(string:sub_word(User, 1, $!), 2),
  do(Socket, "PRIVMSG" ++ " " ++ Channel ++ " " ++ Message),
  do(Socket, "PRIVMSG" ++ " " ++ Nick ++ " " ++ Message).

do(Socket, Command) ->
  case string:right(Command, 2) of
    "\r\n" -> gen_tcp:send(Socket, Command);
    _      -> gen_tcp:send(Socket, string:join([Command, "\r\n"], ""))
  end.

cmd(Pid, Cmd) ->
  Pid ! {cmd, Cmd}.

% msg parser
% read msgs only from the self process?
% handle each msg to a separate proc?
