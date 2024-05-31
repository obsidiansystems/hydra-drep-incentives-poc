# Contribution Guide <!-- omit in toc -->

Contributions and issue reports are encouraged and appreciated!

- [Opening Issues](#opening-issues)
- [Submitting Changes](#submitting-changes)
  - [Guidelines for Commit Messages](#guidelines-for-commit-messages)
    - [Summary Line](#summary-line)
      - [Note on bumping dependencies](#note-on-bumping-dependencies)
    - [Body](#body)
  - [Guidelines for Pull Requests](#guidelines-for-pull-requests)
  - [Code Quality](#code-quality)
    - [Warnings](#warnings)
    - [Build and Test](#build-and-test)
  - [Documentation](#documentation)
    - [In the code](#in-the-code)
    - [In the Readme](#in-the-readme)
- [Development Environment](#development-environment)
  - [Testing features from an unmerged branch](#testing-features-from-an-unmerged-branch)
  - [Hacking on Obelisk from within an Obelisk project](#hacking-on-obelisk-from-within-an-obelisk-project)

## Opening Issues

Before opening an issue, please check whether your issue has already been reported. Assuming it has not:

* Describe the issue you're encountering or the suggestion you're making
* Include any relevant steps to reproduce or code samples you can. It's always easier for us to debug if we have something that demonstrates the error.
* Let us know what version of this project you were using. If you're using a github checkout, provide the git hash.

## Submitting Changes

Most pull requests should target the `develop` branch. `master` is the release branch. `develop` is periodically merged into master after a period of testing.

### Guidelines for Commit Messages

#### Summary Line
The summary line of your commit message should summarize the changes being made. Commit messages should be written in the imperative mood and should describe what happens when the commit is applied. If your commit modifies one of the in-tree haskell packages (found in `./lib`), please prefix your commit summary with the name of the package being modified.

One way to think about it is that your commit message should be able to complete the sentence:
"When applied, this commit will..."

##### Note on bumping dependencies
Commits that update a dependency should include some information about why the dependency was updated in the commit message.

#### Body
For breaking changes, new features, refactors, or other major changes, the body of the commit message should describe the motivation behind the change in greater detail and may include references to the issue tracker. The body shouldn't repeat code/comments from the diff.

### Guidelines for Pull Requests

Wherever possible, pull requests should add a single feature or fix a single bug. Pull requests should not bundle several unrelated changes.


### Code Quality

#### Warnings

Your pull request should add no new warnings to the project. It should also generally not disable any warnings.

#### Build and Test

Make sure the project builds and that the tests pass! This will generally also be checked by CI before merge, but trying it yourself first means you'll catch problems earlier and your contribution can be merged that much sooner!

To test that your changes build across platforms, you can also try to build release.nix, like this:
```bash
nix-build release.nix
```

Note, however, that to build release.nix you must accept the android license agreement and your machine must be configured to build both ios and android executables (usually via remote builders).


### Documentation

#### In the code
We're always striving to improve documentation. Please include [haddock](https://haskell-haddock.readthedocs.io/en/latest/index.html) documentation for any added code, and update the documentation for any code you modify.

#### In the [Readme](README.md)
The readme is the first place a lot of people look for information about the repository. Update any parts of the readme that are affected by your PR.

### Hacking on Obelisk from within an Obelisk project

`ob` will defer to the version found in your project's `.obelisk/impl` directory. To work on that version specifically:

```bash
ob thunk unpack ./.obelisk/impl
cd ./.obelisk/impl
# apply your changes
```

If you want to commit your changes, first push them to your fork of obelisk and then

```bash
cd /your/project/root
ob thunk pack .obelisk/impl
git add .obelisk/impl
git commit -m "Bump obelisk"
```
