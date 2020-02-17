# elm-code-explorer

Dashboard for navigating Elm code and understanding the AST.

## How it works

This is a regular Elm setup, where `./src/Main.elm` is your regular project.

**Important: only single-file projects are supported at the moment.**

You can explore the structure of this file by running `npm install` and `npm start`, which spins up a Node server that serves the Elm app from `./src/Dashboard.elm`. This app fetches the source code of `./src/Main.elm`, runs [elm-syntax](https://package.elm-lang.org/packages/stil4m/elm-syntax/latest/), and builds an interactive view of the AST.
