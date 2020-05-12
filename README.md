# clj-ml

[![Clojars Project](https://img.shields.io/clojars/v/org.clojars.punit-naik/clj-ml.svg)](https://clojars.org/org.clojars.punit-naik/clj-ml)

Collection of ML and it's corresponsing utilities in Clojure

## TODO/CURRENT ISSUES

1. have to add visualisation utilities for displaying data as graphs, charts, etc,.

## Usage

**NOTE**: [Java](https://openjdk.java.net/) and [Leiningen](https://github.com/technomancy/leiningen) must be pre-installed!

### Test

```
lein test
```

### Build

```
lein with-profiles +uberjar uberjar
```

## Docs

[`clj-ml` API Docs](https://punit-naik.github.io/clj-ml)

### Code Coverage

[`clj-ml` Code Coverage](https://punit-naik.github.io/clj-ml/coverage/)

## Note

1. Sorry for the extremely verbose code in some cases. I have coded it that way so that I understand the steps properly, especially in some cases where complex mathematical steps are involved. But it's no excuse for not writing idiomatic code, I get it.
2. I have not tested the functions inside this library for all possible inputs. Will look to improve the code incrementally.

## License

Copyright © 2020 [Punit Naik](https://github.com/punit-naik)

This program and the accompanying materials are made available under the
terms of the Eclipse Public License 2.0 which is available at
http://www.eclipse.org/legal/epl-2.0.

This Source Code may also be made available under the following Secondary
Licenses when the conditions for such availability set forth in the Eclipse
Public License, v. 2.0 are satisfied: GNU General Public License as published by
the Free Software Foundation, either version 2 of the License, or (at your
option) any later version, with the GNU Classpath Exception which is available
at https://www.gnu.org/software/classpath/license.html.
