-module(zazu).
-export([start/4, do/2, kill/1]).

start(Mode, Host, Port, Nick) ->
  spawn(fun() -> connect(Mode, Host, Port, Nick) end).

do(Pid, Cmd) ->
  Pid ! {cmd, Cmd}.

kill(Pid) ->
  Pid ! quit.

connect(tcp, Host, Port, Nick) ->
  {ok, Socket} = gen_tcp:connect(Host, Port, [{keepalive, true}]),
  send_packet({tcp, Socket}, "NICK " ++ Nick),
  send_packet({tcp, Socket}, "USER ykz 0 * :" ++ Nick),
  loop({tcp, Socket});
connect(ssl, Host, Port, Nick) ->
  ssl:start(),
  {ok, Socket} = ssl:connect(Host, Port, []),
  send_packet({ssl, Socket}, "NICK " ++ Nick),
  send_packet({ssl, Socket}, "USER ykz 0 * :" ++ Nick),
  loop({ssl, Socket}).

loop({Mode, Socket}) ->
  receive
    {Mode, _, Msg} ->
      io:format("~p~n", [Msg]),
      handle_packet({Mode, Socket}, Msg),
      loop({Mode, Socket});
    {cmd, Command} ->
      send_packet({Mode, Socket}, Command),
      loop({Mode, Socket});
    quit ->
      send_packet({Mode, Socket}, "QUIT :killed from my master"),
      case Mode of
        tcp -> gen_tcp:close(Socket);
        ssl -> ssl:close(Socket)
      end
  end.

handle_packet({Mode, Socket}, Msg) ->
  case string:tokens(Msg, " ") of
    ["PING"|_] ->
      send_packet({Mode, Socket}, re:replace(Msg, "PING", "PONG", [{return, list}]));
    [User, "PRIVMSG", Channel|[H|Message]] when H == ":zazu"; H == ":zazu:" ->
      io:format("~p~n", [Message]), % dev
      handle_msg({Mode, Socket}, User, Channel, zazu_helper:strip_msg(Message));
    _ ->
      loop({Mode, Socket})
  end.

handle_msg({Mode, Socket}, User, Channel, [H|_]) when H == "malaka" ->
  send_packet({Mode, Socket}, reply({targeted, Channel, zazu_helper:fetch_nick(User), "gamiesai"}));
handle_msg({Mode, Socket}, User, Channel, [H|T]) when H == "announce" ->
  inets:start(),
  httpc:request(
    post,
    {"http://0.0.0.0:3030/widgets/welcome", [], "application/x-www-formurlencoded", "\{ \"auth_token\": \"YOUR_AUTH_TOKEN\", \"text\": \"" ++ string:join(T, " ") ++ "\" \}" },
    [], []
  ),
  send_packet({Mode, Socket}, reply({targeted, Channel, zazu_helper:fetch_nick(User), "announced"}));
handle_msg({Mode, Socket}, User, Channel, _Msg) ->
  send_packet({Mode, Socket}, reply({public, Channel, "ase mas re " ++ zazu_helper:fetch_nick(User)})).

reply({targeted, Channel, Nick, Answer}) ->
  "PRIVMSG" ++ " " ++ Channel ++ " " ++ ":" ++ Nick ++ " " ++ Answer;
reply({public, Channel, Answer}) ->
  "PRIVMSG" ++ " " ++ Channel ++ " " ++ ":" ++ Answer.

send_packet({tcp, Socket}, Command) ->
  case string:right(Command, 2) of
    "\r\n" -> gen_tcp:send(Socket, Command);
    _      -> gen_tcp:send(Socket, string:join([Command, "\r\n"], ""))
  end;
send_packet({ssl, Socket}, Command) ->
  case string:right(Command, 2) of
    "\r\n" -> ssl:send(Socket, Command);
    _      -> ssl:send(Socket, string:join([Command, "\r\n"], ""))
  end.
