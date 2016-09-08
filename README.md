Building
========

I use version 9.9.9 of `stack` to build this. Call `stack --version` to see what
version you are using.

First, `stack init --solver`. I have to add the `--solver` to tell `stack` to
ask `cabal` for a build plan - otherwise it can't find the `gll` library (which
is fairly new/unused).

Then `stack build`.
