# purescript-package-graph

Tool to help the PureScript community determine which library one should next update, so that the ecosystem is updated for the upcoming `v0.14.0` PureScript release.

## Setup

### Getting the Current Release Candidate for `v0.14.0`

1. Install `stack`
1. Clone the [`purescript`](https://github.com/purescript/purescript) repo
1. Run `stack setup`
1. Run `stack build`

The binary will be found in `.stack-work/install/<more directories>/bin/purs`

### Build this tool

1. `git clone` this repo
1. Run `npm i purescript spago`
1. Run `npx spago bundle-app -t ./package-graph.js`

### Install `dhall-json`

1. Follow the [installation instructions](https://docs.dhall-lang.org/tutorials/Getting-started_Generate-JSON-or-YAML.html#installation) for installing `dhall`.

### Use this tool to determine which library to update next

1. `git clone` the [package-sets repo's `prepare-0.14` branch]()
1. `cd` into the `src`
1. Convert the `packages.dhall` file into a JSON version by running:
    - `dhall-to-json --compact --file ./packages.dhall --output ./packageSet.json`
1. Output an ordered list of packages with all their transitive dependencies by running:
    - `node ./package-graph.js --input ./packageSet.json genLibDeps -o libDeps.txt`
1. Generate a custom `spago.dhall` file that only builds the given package after which it is named (e.g. `prelude.dhall` when building the `prelude` package) by running the below command. Note: use the `--whitelist`/`-w` option to only output the desired packages rather than the entire package set, which is the default:
    - `node ./package-graph.js --input ./packageSet.json genSpagoFiles -d ./spagoFiles -w prelude,foldable-traversable`
