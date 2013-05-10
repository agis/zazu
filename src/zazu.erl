-module(zazu).
-export([start/3, do/2, kill/1]).
-export([start/0]).

start(Host, Port, Nick) ->
  spawn(fun() -> connect(Host, Port, Nick) end).

% dev
start() ->
  spawn(fun() -> connect("127.0.0.1", 6667, "zazu") end).

do(Pid, Cmd) ->
  Pid ! Cmd.

kill(Pid) ->
  Pid ! quit.

connect(Host, Port, Nick) ->
  {ok, Socket} = gen_tcp:connect(Host, Port, [{keepalive, true}]),
  send_tcp(Socket, string:join(["NICK", Nick], " ")),
  send_tcp(Socket, string:join(["USER", Nick, "0 * :zazu bot"], " ")),
  send_tcp(Socket, "join #hi"), % dev
  loop(Socket).

loop(Socket) ->
  receive
    {tcp, _, Msg} ->
      io:format("~p~n", [Msg]),
      handle_tcp(Socket, Msg),
      loop(Socket);
    quit ->
      send_tcp(Socket, "QUIT :killed from my master"),
      gen_tcp:close(Socket);
    Command ->
      send_tcp(Socket, Command),
      loop(Socket)
  end.

% Handles incoming TCP messages   
handle_tcp(Socket, Msg) ->
  case string:tokens(Msg, " ") of
    ["PING"|_] ->
      send_tcp(Socket, re:replace(Msg, "PING", "PONG", [{return, list}]));
    [User, "PRIVMSG", Channel|[":zazu"|Message]] ->
      io:format("~p~n", [Message]), % dev
      handle_msg(Socket, User, Channel, zazu_helper:strip_msg(Message));
    _ ->
      loop(Socket)
  end.

% Handles recognized incoming messages
handle_msg(Socket, User, Channel, [H|_]) when H == "malaka" ->
  send_tcp(Socket, reply({targeted, Channel, zazu_helper:fetch_nick(User), "gamiesai"}));
handle_msg(Socket, User, Channel, [H|T]) when H == "announce" ->
  inets:start(),
  httpc:request(post, { "http://0.0.0.0:3030/widgets/welcome", [], "application/x-www-formurlencoded", "\{ \"auth_token\": \"YOUR_AUTH_TOKEN\", \"text\": \"" ++ zazu_helper:construct_message(T) ++ "\" \}" }, [], []),
  send_tcp(Socket, reply({targeted, Channel, zazu_helper:fetch_nick(User), "announced"}));
handle_msg(Socket, _User, Channel, _Msg) ->
  send_tcp(Socket, reply({public, Channel, "unrecognized command"})).

% Constructs IRC PRIVMSG replies to send to the server
reply({targeted, Channel, Nick, Answer}) ->
  "PRIVMSG" ++ " " ++ Channel ++ " " ++ ":" ++ Nick ++ " " ++ Answer;
reply({public, Channel, Answer}) ->
  "PRIVMSG" ++ " " ++ Channel ++ " " ++ ":" ++ Answer.

% Normalizes and sends a TCP packet to the server
send_tcp(Socket, Command) ->
  case string:right(Command, 2) of
    "\r\n" -> gen_tcp:send(Socket, Command);
    _      -> gen_tcp:send(Socket, string:join([Command, "\r\n"], ""))
  end.
