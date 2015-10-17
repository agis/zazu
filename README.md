# Zazu, your majordomo

Zazu is an experimental IRC bot that does nothing interesting yet. And
probably will never do.

### Usage

From the root folder, fire up a Erlang shell and build Zazu:

```erlang
> make:all().
```

spawn it:

```erlang
> Bot = zazu:start(tcp, "irc.freenode.net", 6667, "zazu").
```

this will spawn a new VM process and save it into `Bot`. From now on you can execute ad-hoc IRC commands:

```erlang
> zazu:do(Bot, "join #ye").
```

For an SSL connection:

```erlang
> Bot = zazu:start(ssl, "irc.freenode.net", 6667, "zazu").
```


The same way, you can spawn as many bots as you like and operate them from the same shell.

### Features

* Announcing updates on [dashing](http://shopify.github.io/dashing/) dashboards.
* Easily extended: you can easily add your own command handlers, see `zazu:handle_msg/4`. Just pattern-match as you wish :)
* Supports SSL.
* Process-based. From the same shell you can spawn as many bots as you like and administrate them at the same time.

### Contributing

I'd be more than happy to see issues reported, pull requests opened, wiki pages added.
