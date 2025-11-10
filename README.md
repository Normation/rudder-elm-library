# Rudder Elm library

Our Elm "library" for sharing code : 
* between [Rudder](https://github.com/Normation/rudder) and [plugins](https://github.com/Normation/rudder-plugins)
* factorized data types and logics, split into modules (to a certain extent, some are "components" which require [Nested TEA](https://sporto.github.io/elm-patterns/architecture/nested-tea.html))

## Installation
This git repo is installed using [elm-git-install](https://github.com/robinheghan/elm-git-install) :
* to have our own versioning semantic, and not the one enforced by Elm (["Semantic versioning"](https://elm-lang.org/))
* to be able to facilitate development on the library by changing git branch

To install :
* add a `elm-git.json` file next to the `elm.json` file, with the desired version of this library :
  ```json
  {
    "git-dependencies": {
      "direct": {
        "https://github.com/Normation/rudder-elm-library.git": "x.x.x"
      },
      "indirect": {}
    }
  }
  ```
  
* add the `elm-github-install` [npm package](https://www.npmjs.com/package/elm-github-install) to your npm project
* add an npm script to run `elm-git-install` :
  ```json
  {
    "scripts": {
      "elm-git-install": "cd elm && elm-git-install"
    },
    ...
  }
  ```
* add `npm run elm-git-install` to your build script


## Caveats

* Elm dependency management is not automatic when installing this library, it will require dependencies of modules that are imported to be installed
