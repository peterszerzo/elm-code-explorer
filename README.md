# 🗺️ elm-code-explorer

Dashboard for navigating Elm code and understanding the AST.

## Motivation

If you struggle to name pieces of code accurately (declaration, expression, literal et al.), or feel that inaccurate/inconsistent names hurt the quality of communication with your peers, you are not alone.

This little app sheds some light on these naming finesses in Elm code: just click on a variable or function signature, navigate up and down the node of the corresponding AST, and enjoy some cross-highlighting. Check it out:

## How it works

This is a regular Elm setup with a [src/Main.elm](src/Main.elm) entry point. You can edit and run this file with the build tool of your choosing. However, you can also explore the AST of this file by running `npm install` and `npm start`, which spins up a Node server that serves the Elm app defined in the `./explorer` folder.

This app fetches the source code of `./src/Main.elm`, runs [elm-syntax](https://package.elm-lang.org/packages/stil4m/elm-syntax/latest/), and builds an interactive view of the AST.

**Important: only single-file projects are supported at the moment.**

## License

MIT.
