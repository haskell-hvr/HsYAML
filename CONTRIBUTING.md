#  Contribution Guidelines

## Issue Tracker & Bug Reporting

- Try to provide simple examples demonstrating the issue at hand; describe what happened as well as what you would have expected to happen instead.

- Reproduction-instructions shall be expressed in terms of Haskell.org's standard tooling (i.e. `cabal`).

- Be aware this is a F/OSS project; the software is provided "as is" for no charge and you're entitled in terms of support to what you payed for.

## Coding Guidelines

- Try to follow the pre-existing indentation style; avoid tabs for indentation; avoid trailing whitespace; if in doubt, try to follow [this style guide](https://github.com/hvr/haskell-style-guide/blob/master/haskell-style.md)

- Development & support is done primarily using Haskell.org's standard tooling, i.e. `cabal`; we don't have time & energy to support everyone's favourite third-party tooling.

- Haskell packages are subject to the [Haskell Package Versioning Policy](https://pvp.haskell.org/) which governs both the API versioning as well as the specification of dependency versions.

- Generally, only properly released major versions of tools and libraries are actively supported in Hackage releases; this is reflected in the dependency version constraints (see also previous item).

- When relaxing upper bounds for dependencies in order to declare compatibility with new major versions, it's **not** sufficient to merely rely on CI; make sure to review the API changes prompting the major version increment to ensure that the version relaxation is indeed safe/sound.

## Pull Request Process

### We Use [Github Flow](https://guides.github.com/introduction/flow/index.html), So All Code Changes Happen Through Pull Requests

Pull requests are the best way to propose changes to the codebase (we use [Github Flow](https://guides.github.com/introduction/flow/index.html)). 
We actively welcome your pull requests:

1. Fork the repo and create your branch from `master`.
2. If you've changed APIs, update the documentation.
3. Ensure the test suite passes. See the testing-process below.
4. Issue that pull request!

### Testing

Run the following list of commands to see if the changes satisfy the tests

* Browse to the directory where all the files of this repository are located.  
* ```cabal test```

HsYAML is also tested using [YAML-Test-Suite](https://github.com/yaml/yaml-test-suite). 
Run the following list of commands to see the updated performance on YAML-Test-Suite and mention the results in the pull request.

* Browse to the directory where all the files of this repository are located.  
* Download/Clone the [YAML-Test-Suite](https://github.com/yaml/yaml-test-suite) repository.
```
git clone https://github.com/yaml/yaml-test-suite.git
```
* Run the following commands to see the updated results
```
cabal run yaml-test run-tml yaml-test-suite/test/*.tml
```
You will see some output like
```
done -- passed: 316 (ev: 32, ev+json: 93, ev+json+yaml: 120, err: 71) / failed: 2 (err: 2, ev:0, json:0, yaml:0, ok:0)
```