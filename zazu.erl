-module(zazu).
-export([start/3, do/2, kill/1]).
-compile(export_all).

start(Host, Port, Nick) ->
  spawn(fun() -> connect(Host, Port, Nick) end).

% for development only
start() ->
  spawn(fun() -> connect("127.0.0.1", 6667, "rze") end).

connect(Host, Port, Nick) ->
  {ok, Socket} = gen_tcp:connect(Host, Port, [{keepalive, true}]),
  cmd(Socket, string:join(["NICK", Nick], " ")),
  cmd(Socket, string:join(["USER", Nick, "0 * :zazu bot"], " ")),
  cmd(Socket, "join #hi"), % for development only
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

% Handles incoming TCP messages   
handle(Socket, Msg) ->
  case string:tokens(Msg, " ") of
    ["PING"|_] ->
      cmd(Socket, re:replace(Msg, "PING", "PONG", [{return, list}]));
    [User, "PRIVMSG", Channel|[":zazu"|Message]] ->
      io:format("~p~n", [Message]), % for debugging
      handle(Socket, User, Channel, normalize_inc(Message));
    _ ->
      loop(Socket)
  end.

% Handles recognized incoming messages
handle(Socket, User, Channel, Msg) when Msg == "malaka" ->
  cmd(Socket, reply({targeted, Channel, fetch_nick(User), "gamiesai"}));
handle(Socket, _User, Channel, _Msg) ->
  cmd(Socket, reply({public, Channel, "unrecognized command"})).

% Constructs commands to send to the server
reply({targeted, Channel, Nick, Answer}) ->
  "PRIVMSG" ++ " " ++ Channel ++ " " ++ ":" ++ Nick ++ " " ++ Answer;
reply({public, Channel, Answer}) ->
  "PRIVMSG" ++ " " ++ Channel ++ " " ++ ":" ++ Answer.

fetch_nick(User) ->
  string:sub_string(string:sub_word(User, 1, $!), 2).

% Strips trailing newlines from incoming messages
normalize_inc([Message]) ->
  re:replace(Message, "\r\n", "", [{return, list}]).

% Normalizes and sends a TCP packet to the server
cmd(Socket, Command) ->
  case string:right(Command, 2) of
    "\r\n" -> gen_tcp:send(Socket, Command);
    _      -> gen_tcp:send(Socket, string:join([Command, "\r\n"], ""))
  end.

do(Pid, Cmd) ->
  Pid ! Cmd.

kill(Pid) ->
  Pid ! quit.

% add documentation
% read msgs only from the self process?
% handle each msg to a separate proc?
