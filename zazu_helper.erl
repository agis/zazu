-module(zazu_helper).
-compile(export_all).

fetch_nick(User) ->
  string:sub_string(string:sub_word(User, 1, $!), 2).

% Accepts an incoming message as a list and strips trailing newlines
strip_msg(Message) ->
  Strip = fun(X) -> re:replace(X, "\r\n", "", [{return, list}]) end,
  lists:map(Strip, Message).

% Joins the elements of a list into a string with spaces
construct_message(L) ->
  string:join(L, " ").
