# lisp-interpreter
A Lisp interpreter written in Clojure.

## Features
- Core functions: arithmetic operations, list, car, cdr, cons, concat, map, apply, print and more.
- Special forms: if, quote, quasi-quote, do, fn (lambda), let, defmacro, define, set!
- Lexical scoping
- Lambda functions
- Macros
- Reader macros: ``` ' ```,``` ` ``` ,``` ~ ```,``` ~@ ```
- REPL

## Usage

1. Uncomment line number 6 and comment line 7 in ```src/lisp_interpreter/core.clj``` if you want to run the REPL.
2. Run the app using ```lein run``` in your terminal from the root directory of the project.

## License

Copyright © 2019 Sunil KS

This program and the accompanying materials are made available under the
terms of the Eclipse Public License 2.0 which is available at
http://www.eclipse.org/legal/epl-2.0.

This Source Code may also be made available under the following Secondary
Licenses when the conditions for such availability set forth in the Eclipse
Public License, v. 2.0 are satisfied: GNU General Public License as published by
the Free Software Foundation, either version 2 of the License, or (at your
option) any later version, with the GNU Classpath Exception which is available
at https://www.gnu.org/software/classpath/license.html.
