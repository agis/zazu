# Zazu, your majordomo

Zazu is an experimental IRC bot written written in Erlang.

### Usage

From the root folder, fire up a Erlang shell and build Zazu:

```
make:all().
```

spawn it:

```
Bot = zazu:start(tcp, "irc.freenode.net", 6667, "zazu").
```

this will spawn a new VM process and save it into `Bot`. From now on you can execute ad-hoc IRC commands:

```
zazu:do(Bot, "join #ye").
```

To connect using SSL, replace `tcp` with `ssl`.

The same way, you can spawn as many bots as you like and operate them from the same shell.

### Features

* Announcing updates on [dashing](http://shopify.github.io/dashing/) dashboards.
* Easily extended: you can easily add your own command handlers, see `zazu:handle_msg/4`. Just pattern-match as you wish :)
* Supports SSL.
* Process-based. From the same shell you can spawn as many bots as you like and administrate them at the same time.

### Documentation

Visit the [wiki](https://github.com/Agis-/zazu/wiki) for documentation.

### Contributing

I'd be more than happy to see issues reported, pull requests opened, wiki pages added.
