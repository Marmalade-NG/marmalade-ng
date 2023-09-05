# Marmalade-NG - Deployment

## Required tools
  - make (already installed on most Unix flavors)
  - m4 (already installed on most Unix flavors)
  - kda tool (https://github.com/kadena-io/kda-tool)
  - optional : jq tool


## Prerequisites
  - An already created namespace (with the associated key)
  - An already on-chain defined keyset (with the associated key)
  - A funded account with enough KDA to pay deployment fees (and the associated key)

All keys must reside in the keys directory in plain YAML format. Using others key types (eg: Chainweaver keys) would require to slightly modify the Makefile.


## Configure the deployment

Modify the data.yaml file according to your parameters:
  - chain
  - network
  - gas paying account
  - keys

Modify the Makefile:
  - `NAMESPACE` variable
  - uncomment the line `INIT=-D__INIT__` if it the first deployment and you need to initialize the modules.
  - List of signing keys

## Generate the transactions files

Just do:
```sh
make
```

## Deploy interfaces
```sh
kda send interface-*.json
```
before proceeding to the next step you can verify that everything is ok and has been deployed.

```sh
for TRX in interface-*.json; do kda poll $TRX |jq;done
```

## Deploy Marmalade core
```sh
kda send core-*.json
```
before proceeding to the next step you can verify that everything is ok and has been deployed.

```sh
for TRX in core-*.json; do kda poll $TRX |jq;done
```

## Deploy policies
```sh
kda send policy-*.json
```
before proceeding to the next step you can verify that everything is ok and has been deployed.

```sh
for TRX in policy-*.json; do kda poll $TRX |jq;done
```

## Deploy std-policies module
```sh
kda send std-policies.json
```
You can verify that the module has been well deployed.

```sh
kda poll std-policies.json
```
