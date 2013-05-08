-module(zazu).
-export([start/3, do/2, kill/1]).

start(Host, Port, Nick) ->
  spawn(fun() -> connect(Host, Port, Nick) end).

connect(Host, Port, Nick) ->
  {ok, Socket} = gen_tcp:connect(Host, Port, [{keepalive, true}]),
  cmd(Socket, string:join(["NICK", Nick], " ")),
  cmd(Socket, string:join(["USER", Nick, "0 * :zazu bot"], " ")),
  loop(Socket).

loop(Socket) ->
  receive
    {tcp, _, Msg} ->
      io:format("~p~n", [Msg]),
      handle(Socket, Msg),
      loop(Socket);
    quit ->
      cmd(Socket, "QUIT :killed from my master"),
      gen_tcp:close(Socket);
    Command ->
      cmd(Socket, Command),
      loop(Socket)
  end.

handle(Socket, Msg) ->
  case string:tokens(Msg, " ") of
    ["PING"|_] ->
      cmd(Socket, re:replace(Msg, "PING", "PONG", [{return, list}]));
    [User, "PRIVMSG", Channel|Message] ->
      handle(Socket, User, Channel, Message);
    _ ->
      loop(Socket)
  end.

handle(Socket, User, Channel, Message) ->
  Nick = string:sub_string(string:sub_word(User, 1, $!), 2),
  cmd(Socket, "PRIVMSG" ++ " " ++ Channel ++ " " ++ Message),
  cmd(Socket, "PRIVMSG" ++ " " ++  Nick   ++ " " ++ Message).

cmd(Socket, Command) ->
  case string:right(Command, 2) of
    "\r\n" -> gen_tcp:send(Socket, Command);
    _      -> gen_tcp:send(Socket, string:join([Command, "\r\n"], ""))
  end.

do(Pid, Cmd) ->
  Pid ! Cmd.

kill(Pid) ->
  Pid ! quit.

% think something useful!!! start with the curl request

% msg parser
% read msgs only from the self process?
% handle each msg to a separate proc?
