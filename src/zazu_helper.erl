-module(zazu_helper).
-compile(export_all).

fetch_nick(User) -> string:sub_string(string:sub_word(User, 1, $!), 2).

strip_msg(L) -> lists:map(fun(X) -> re:replace(X, "\r\n", "", [{return, list}]) end, L).

construct_message(L) -> string:join(L, " ").
