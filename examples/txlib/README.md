# Marmalade NG examples


## Tools
  - Works with kda tool (https://github.com/kadena-io/kda-tool)

## How to use

### Create keys
  Create a bunch of test keys

  ```sh
  kda keygen plain > gas-payer-key.yaml
  kda keygen plain > collection-key.yaml
  kda keygen plain > creator-key.yaml
  kda keygen plain > tmp-key.yaml
  kda keygen plain > user-A.yaml
  kda keygen plain > user-B.yaml
  kda keygen plain > user-C.yaml
  ...
  ```

### Funds Gas payer
  You can use Kadena Faucet (prepend the public gas-payer-key with `k:`)

### Start to play
  Transactions can be built by:

  ```sh
  kda gen -t xxxx/xxx.tkpl -d testnet.yaml

  kda sign tx.yaml -k xxx-key.yaml
  ...
  kda sign tx.yaml -k yyy-key.yaml
  ```

### Test templates

**creation-minting/create-collection.tkpl:**
- Create a collection
- Needed signatures : ``gas-payer-key``, ``collection-key``

**creation-minting/create-token-instant-mint.tkpl:**
- Create a non fungible token and instant mint it to an user.
- Needed signatures : ``gas-payer-key``, ``collection-key``, ``tmp-key``

**creation-minting/create-token-only.tkpl:**
- Only create a token but protect the mint with a guard.
- Needed signatures : ``gas-payer-key``, ``collection-key``, ``tmp-key``

**creation-minting/mint-token.tkpl:**
- Mint a token created with the previous template
- Needed signatures : ``gas-payer-key``, ``creator-key``

**creation-minting/batch-minting.tkpl:**
- Create and mint a collection of 20 NFTS in a single transaction
- Needed signatures : ``gas-payer-key``, ``collection-key``, , ``tmp-key``

**fixed-sale/fixed-sale.tkpl:**
- Start a fixed quote sale
- Needed signatures : ``gas-payer-key``, ``seller-XX.key``

**fixed-sale/buy.tkpl:**
- Complete the fixed quote sale and buy the token
- Needed signatures : ``gas-payer-key``, ``buyer-XX.key``

**auction-sale/auction-sale.tkpl:**
- Start an auction sale
- Needed signatures : ``gas-payer-key``, ``seller-XX.key``

**auction-sale/bid.tkpl:**
- Bid an auction
- Needed signatures : ``gas-payer-key``, ``buyer-XX.key``

**auction-sale/complete.tkpl:**
- Complete and settle an auction sale (after the timeout)
- Needed signatures : ``gas-payer-key``
