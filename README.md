Note: I don't use this anymore. I've transitioned my blog to a 
statically generated site, the source of which can be found [here][].

---

[here]: https://github.com/pbrisbin/pbrisbin.com

# pbrisbin dot com

The source code for my [site][] which is powered by the [Yesod][] web 
framework, written in haskell.

Simple blog site. Markdown-driven, comments, post administration.

### Try it

I don't claim this will work, but anyway...

First, create the database specified in `config/postgresql.yml`.

~~~ { .bash }
$ git clone https://github.com/pbrisbin/devsite && cd ./devsite
$ cabal install --dry-run --verbose # make sure you don't end up in 
                                    # dependency hell
$ yesod devel
~~~

### Notes to would-be forkers

1. This site typically requires the newest possible version of the yesod 
   package set (and sometimes even git HEAD)

2. The various non-standard dependencies in the cabal file are `-any` 
   versioned. This may or may not lead to breakage in your environment.

[site]:  http://pbrisbin.com
[Yesod]: http://docs.yesodweb.com
