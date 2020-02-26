# elm-code-explorer

Dashboard for navigating Elm code and understanding the AST.

## Motivation

If struggle to name pieces of code accurately (declaration, expression, literal et al.), or feel that inaccuracies hurt the quality of communication, you are not alone.

This little app helps shed some light on these finesses of Elm code: just click on a piece of code, navigate up and down the AST, and enjoy some cross-highlighting. Check it out:

**Disclaimer:** I don't claim I'm getting every term right here. If you have suggestions for improvements, please feel free to file an issue.

## How it works

This is a regular Elm setup, where `./src/Main.elm` is your regular project.

**Important: only single-file projects are supported at the moment.**

You can explore the structure of this file by running `npm install` and `npm start`, which spins up a Node server that serves the Elm app from `./src/Explorer.elm`. This app fetches the source code of `./src/Main.elm`, runs [elm-syntax](https://package.elm-lang.org/packages/stil4m/elm-syntax/latest/), and builds an interactive view of the AST.

## License

MIT.
