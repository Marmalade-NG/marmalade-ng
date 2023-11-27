# Marmalade-NG

## Introduction

The project aims to create a fork / rewrite of Marmalade-V2.

## Project goals

### Business code "In policy only"

The goal is to make Marmalade code as modular as possible, by moving all the "business/feature code" to policies.

Each token creator has the possibility to enable or disable individually each policy or combine policies to make its token work exactly as desired.

A token creator has the ability to create its own policies as well, if he wants a fully customized token behavior. Marmalde-ng aims to provide an easy-to-use and safe framework to create a custom policy.
Moreover the creator is able to combine standard policies with custom policies.   

### Strong security

Unlike existing Marmalade V2, with its simple and comprehensive architecture Marmalade-NG provides a strong security and isolation between features. As such most attacks, and re-entrancy issues can be avoided:
   - X-tokens vulnerabilities
   - X-policies vulnerabilities
   - X-functions vulnerabilities.

Marmalade-NG addresses and consider most front-running and DoS risks.

### Simple architecture

Marmalade-NG uses a simple and clean design. The core of Marmalade-NG is contained in only one main module: `ledger`.

No complex modules stack with insane circular dependencies.

## How to start (for developers)

The documentation can be found here:
https://marmalade-ng.readthedocs.io/en/latest/

Specifically, read the Howto chapter. It's a good starting point and exposes the basics of minting, selling, and create custom policies.
https://marmalade-ng.readthedocs.io/en/latest/how-to.html

### In REPL

You can use the Kadena REPL Sandbox as a starting point. I maintain a branch where Marmalade-NG is pre-deployed:
https://github.com/CryptoPascal31/kadena_repl_sandbox/tree/marmalade-ng

An example of token creation and minting is present and is a good starting point.
https://github.com/CryptoPascal31/kadena_repl_sandbox/blob/marmalade-ng/example-marmamalde-ng.repl

The example should work out-of-the-box with the `pact` [1] executable.


### On chain (Testnet)

Marmalade NG is currently deployed on chain on Testnet:
  - chain : **1**
  - namespace : **n_442d3e11cfe0d39859878e5b1520cd8b8c36e5db**

This repository contains many examples of *almost ready-to-be-used* transactions with the `kda`[2] tool. And most explanations are given in the corresponding Readme.
[./examples/txlib](./examples/txlib)

The PoC explorer: https://explorer.marmalade-ng.xyz can be used to check the results.

* [1] https://github.com/kadena-io/pact/releases
* [2] https://github.com/kadena-io/kda-tool/releases
