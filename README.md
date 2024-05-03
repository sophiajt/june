# June, a gradual, safe systems language

## Goals

- Teachability | Learnability | Readability
- Efficiency | Productivity
- Systems and application programming

Also see the [philosophy](docs/philosophy.md) for more info.

## Participating in the closed alpha/beta

* How to file feedback
    * File feedback as issues on the repo: https://github.com/sophiajt/june/issues
    * When you file issues, please also submit the git hash of the revision you've built from

## Project completeness

June is currently *alpha* quality. It's missing important features and may have corner cases when trying to compile valid programs.

## Running the test suite

To run the June test suite, be sure to have the following installed:

* A recent `rustc`
* A recent `cargo`
* A reasonably good `clang` in your path

The June compiler outputs C code, which you can then build with a C-compatible compiler. The test suite assumes `clang` is the compiler that is available. (Note for Windows users: Visual Studio can install 'clang' as an optional install)

## How to compile

June currently provides some rudamentary support for building june projects. We don't yet support creating new june projects. June only supports binary projects at the moment, with a main.june file inside a `main` folder. The project root is marked by a `June.toml` file which is currently empty and unused, but will eventually contain metadata about the project including the output binary name. Inside of a properly setup june project one should be able to build by simply typing `june build`. Once the build step completes you should have a binary in `build/debug` called`main` which you can run manually.

```
project_dir/
    main/
        main.june
    June.toml
    .gitignore
    build/
```

Alternatively, the current version of June can be used compile June code to C by passing the filename directly such as with `june main.june`. After outputting C, you'll need to redirect this to a file, and then use a C compiler to build the June application.

## The June language

For more information on the June language, check out [the June language documentation](docs/language.md)
