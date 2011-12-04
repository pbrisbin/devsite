# pbrisbin dot com

The source code for my [site][] which is powered by the [Yesod][] web 
framework, written in haskell.

### Notes to would-be forkers

1. This site typically requires the newest possible version of the yesod 
   package set (and sometimes event git HEAD)

2. This site uses newer versions of my [goodies][] and [comments][] 
   packages which (for the time being) must be installed from github.

3. Most of the dependencies in the cabal file are `-any` versioned. This 
   may or may not lead to breakage in your environment.

[site]:     http://pbrisbin.com
[Yesod]:    http://docs.yesodweb.com
[goodies]:  https://github.com/pbrisbin/yesod-goodies
[comments]: https://github.com/pbrisbin/yesod-comments
