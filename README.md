# Marmalade-NG

## Introduction

The project aims to create a fork / rewrite of Marmalade-V2.

## Project goals

### Business code "In policy only"

The goal is to make Marmalade code as modular as possible, by moving all the "business/feature code" to policies.

Each token creator has the possibility to enable or disable individually each policy or combine policies to make its token work exactly as desired.

A token creator has the ability to create its own policies as well, if he wants a fully customized token beahviour. Marmalde-ng aims to provide an easy-to-use and safe framework to create a custom policy.
Moreover the creator is able to combine standard policies with custom policies.   

### Strong security

Unlike existing Marmalade V2, with its simple and comprehensive architecture Marmalade-ng provides a strong security and isolation between features. As such most attacks, and reentrancy issues can be avoided:
   - X-tokens vulnerabilities
   - X-policies vulnerabilities
   - X-functions vulnerabiliies.

Unlike existing Marmalade V2, Marmalade-NG addresses and consider most front-running and DoS risks.

### Simple architecture

Marmalade-NG uses a simple and clean design. The core of Marmalade-NG is contained in only 1 module: `ledger`.

No complex modules stack with insane circular dependencies.

### TODO

Remove the `ACCOUNT_GUARD` event from the ledger interface.
