# [Monadoc](https://www.monadoc.com)

[![build badge][]][build status]

Better Haskell documentation.

This is still a work in progress. I've made a bunch of proofs of concept before. This one is meant to be a real service. Here's a summary of the previous attempts:

- https://github.com/tfausak/grawlix/tree/3189fa2: This introduced the basic idea of commenting on Haskell documentation. It started life as a bookmarklet. I briefly considered making browser extensions, but I figured that wouldn't be able to get enough traction. So I started on the path of parsing the package index and building my own subset of the documentation. I made it a fair ways in that direction, but stopped for some reason.

- https://github.com/tfausak/monadoc-1/tree/37e997f: This one tries to avoid the mess of the package index by proxying Hackage and overlaying commentary on it. Not a bad idea, but puts itself completely at the whims of Hackage. Also it requires dealing with the HTML output of Haddock, which is probably worse than dealing with the modules themselves.

- https://github.com/tfausak/monadoc-2/tree/dac8787: This was exploring a different direction for infrastructure. Instead of running a typical server, this tried to set things up as an AWS Lambda behind an API Gateway. Although it was interesting and not as difficult as I expected, I decided against this because it's hard to deal with locally.

- https://github.com/tfausak/monadoc-3/tree/90e36c1: I felt like the previous attempt spent too much time looking at infrastructure, so this tried to keep everything simple and live in a single file. The end result kind of crumbled under its own weight, but I did discover a lot of good things about the package index.

- https://github.com/tfausak/monadoc-4/tree/86809fc: This attempt tried to focus on building a real service. By that I mean it deployed to AWS and was a site you could actually visit. It also implement login with GitHub, which allowed people to log in. Ultimately I ran into a roadblock with using GHC to parse Haskell files.

I'm kind of surprised I've been at this in one way or another for so long. But at this point I feel like I have all the pieces of the puzzle: I know how to deal with the Hackage index, I know how to parse files with GHC, I can support logging in with GitHub, and I can deploy the whole thing to AWS. All that's left is to assemble that into a working product :)

Here's a wishlist of features that I want to implement:

- Basic pages for Hackage users, packages, versions, revisions, and modules, just like [Hackage](https://hackage.haskell.org/).
- Pages for individual identifiers as well, like [ClojureDocs](https://clojuredocs.org/clojure.core/map). (As opposed or in addition to `#anchor` links on a module page.)
- Inline source view like [APIdock](https://apidock.com/ruby/Enumerable/map).
- Comments on anything (but especially identifiers) like [PHP](https://www.php.net/manual/en/function.str-pad).
- Live search like on [CocoaPods](https://cocoapods.org/).
- Various maintainer tools like reverse dependencies (<https://packdeps.haskellers.com/>), licenses, and feeds/notifications about versions/comments.

In addition, I want this project to be easy to hack on. That means it should build easily on Linux, macOS, and Windows with both Cabal and Stack. It shouldn't require any special infrastructure like Docker or Nix. It should prefer simpler code so that anyone can jump in. Whenever possible things should be tested and enforced by CI.

[build badge]: https://github.com/tfausak/monadoc/workflows/.github/workflows/ci.yaml/badge.svg
[build status]: https://github.com/tfausak/monadoc/actions
