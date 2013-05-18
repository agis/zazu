# Zazu, your majordomo

Zazu is an experimental IRC bot aiming to be easy to extend.

## Usage

From the root folder, fire ap a Erlang shell and build it:

```
make:all().
```

spawn it:

```
Bot = zazu:start(tcp, "irc.freenode.net", 6667, "zazu").
```

`start/4` will spawn a new VM process. From now on you can execute ad-hoc IRC commands:

```
zazu:do(Bot, "join #ye").
```

To connect using SSL, replace `tcp` with `ssl`.

So you can spawn as many bots as you like and operate them from the same shell.

## Features

* Announcing updates on [dashing](http://shopify.github.io/dashing/) dashboards.
* Easily extended: you can easily add your own command handlers, see `zazu:handle_msg/4`. Just pattern-match as you wish :)
* Supports SSL.
* Process-based. From the same shell you can spawn as many bots as you like and administrate them at the same time.





