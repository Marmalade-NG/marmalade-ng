Ledger: Reference and API
=========================

This section only lists the most important functions that are intended to be called externally.


Token creation
--------------
create-token-id
~~~~~~~~~~~~~~~
*token-uri* ``string`` *creation-guard* ``guard``  *→* ``string``

Create a tokenId from an URI and a creation guard. This function does not write anything to tables.
It's just a pure function that allows a token creator to compute the token-id in advance.

.. code:: lisp

  (create-token-id  (read-keyset 'c-ks) "https://red-tulips.com/tulip-1")
   > "t:_yTD6obGrZk07HIEDaZ9pTwKce2Swv4NMN7DhjzjCbI"


create-token
~~~~~~~~~~~~
*token-id* ``string`` *precision* ``integer`` *uri* ``string`` *policies* ``[module{token-policy-ng-v1}]`` *creation-guard* ``guard``  *→* ``bool``

Create a token and store it into the ledger. The token-id should have been precomputed.

The list of policies can be generated using the helpers function from ``std-policies`` module.

The creation-guard is enforced and can be scoped by the capability ``(ENFORCE-RESERVED)``.

The hooks ``(enforce-init)`` of the policies are called during the creation.

.. code:: lisp

  (create-token ""t:_yTD6obGrZk07HIEDaZ9pTwKce2Swv4NMN7DhjzjCbI"" 0
                 "https://red-tulips.com/tulip-1"
                 [marmalade-ng.policy-collection, marmalade-ng.policy-instant-mint]
                 (read-keyset 'c-ks))


Tokens management
-----------------

.. _LEDGER-GET-POLICIES:

get-policies
~~~~~~~~~~~~
*token-id* ``string`` *→* ``[module{token-policy-ng-v1}]``

Return the policies associated with a token. The list of policies can be made more human readable
using the ``std-policy`` module.

.. code:: lisp

  (marmalade-ng.std-policies.from-policies (get-policies "t:_yTD6obGrZk07HIEDaZ9pTwKce2Swv4NMN7DhjzjCbI"))
    > "COLLECTION INSTANT-MINT"

.. _LEDGER-GET-URI:

get-uri
~~~~~~~~~~~~
*token-id* ``string`` *→* ``string``

Return the URI associated with a token.

.. code:: lisp

  (get-uri "t:_yTD6obGrZk07HIEDaZ9pTwKce2Swv4NMN7DhjzjCbI")
    > "https://red-tulips.com/tulip-1"

.. _LEDGER-PRECISION:

precision
~~~~~~~~~
*token-id* ``string`` *→* ``integer``

Return the precsion of a token

.. code:: lisp

  (get-precision "t:_yTD6obGrZk07HIEDaZ9pTwKce2Swv4NMN7DhjzjCbI")
    > 0

.. _LEDGER-TOTAL-SUPPLY:

total-supply
~~~~~~~~~~~~
*token-id* ``string`` *→* ``integer``

Return the total supply of a token (ie: minted - burned).

.. code:: lisp

  (total-supply "t:_yTD6obGrZk07HIEDaZ9pTwKce2Swv4NMN7DhjzjCbI")
    > 3.0

token-exist
~~~~~~~~~~~~
*token-id* ``string`` *→* ``bool``

Returns true if the token exists.

.. code:: lisp

  (token-exist "t:_yTD6obGrZk07HIEDaZ9pTwKce2Swv4NMN7DhjzjCbI")
    > true

list-holders
~~~~~~~~~~~~
*token-id* ``string`` *→* ``[object]``

Return the lists of accounts holding the token id.

The result is a list of object with the 2 fields ``account`` and ``balance``.

**Important**: Local only function. Do not use in transactions.

.. code:: lisp

  (list-holders "t:_yTD6obGrZk07HIEDaZ9pTwKce2Swv4NMN7DhjzjCbI")


.. code-block::

  [ {"account":"r:user.pascal",
     "balance":1.0},
    {"account":"k:0c1cb582d44802541d905c8b0db25faeabfd6ebf1b20355d9fae1d4de37d590d",
     "balance":2.0}
  ]


Accounts management
--------------------
create-account
~~~~~~~~~~~~~~
*id* ``string`` *account* ``string`` **guard** ``guard`` *→* ``bool``

Create an account for a given token. If the account is a principal name, principal is enforced.


.. code:: lisp

  (create-account "t:_yTD6obGrZk07HIEDaZ9pTwKce2Swv4NMN7DhjzjCbI" "r:user.pascal" (keyset-ref-guard "user.pascal"))
    > true

get-balance
~~~~~~~~~~~
*id* ``string`` *account* ``string`` *→* ``decimal``

Return the balance of an account for a given token-id.

.. code:: lisp

  (get-balance "t:_yTD6obGrZk07HIEDaZ9pTwKce2Swv4NMN7DhjzjCbI" "r:user.pascal")
    > 1.0

.. _LEDGER-TOTAL-ACCOUNT-GUARD:

account-guard
~~~~~~~~~~~~~
*id* ``string`` *account* ``string`` *→* ``guard``

Return the guard of an account for the given token-id.

.. code:: lisp

  (account-guard "t:_yTD6obGrZk07HIEDaZ9pTwKce2Swv4NMN7DhjzjCbI" "r:user.pascal")
    >  user.pascal

account-exist
~~~~~~~~~~~~~
*id* ``string`` *account* ``string`` *→* ``bool``

Return true if the account exists for a given token-id.

.. code:: lisp

  (account-exist "t:_yTD6obGrZk07HIEDaZ9pTwKce2Swv4NMN7DhjzjCbI" "r:user.pascal")
    > true

details
~~~~~~~
*id* ``string`` *account* ``string`` *→* ``object{account-details}``

Return the account details for a given token-id.

.. code:: lisp

  (details "t:_yTD6obGrZk07HIEDaZ9pTwKce2Swv4NMN7DhjzjCbI" "r:user.pascal")

.. code::

    > {"id": "t:_yTD6obGrZk07HIEDaZ9pTwKce2Swv4NMN7DhjzjCbI",
       "account": "r:user.pascal",
       "balance": 1.0,
       "guard": {"keysetref":{"ns":"user","ksn":"pascal"}}
      }

list-balances
~~~~~~~~~~~~~
*account* ``string`` *→* ``[object]``

Return the list of tokens and corresponding balances owned by an account.

**Important**: Local only function. Do not use in transactions.

.. code:: lisp

  (list-balance "r:user.pascal")

.. code::

    > [{"id": "t:_yTD6obGrZk07HIEDaZ9pTwKce2Swv4NMN7DhjzjCbI"
        "balance": 1.0},
       {"id": "t:422uU9AJHLeLr6iPGHCkUA_eIvTboKKp5dXaUmctCAw"
        "balance": 2.0},
      ]


Standard operations
-------------------

The standard operations are those defined in the interface `poly-fungible-v3`

Most functions are similar to Marmalade V1.

mint
~~~~
*id* ``string`` *account* ``string`` *guard* ``guard`` *amount* ``decimal`` *→* ``bool``

Mint an *amount* of a token *id* for *account*.

The token must have been created previously.

The function calls the hooks ``(enforce-mint)`` from all policies associated with the token.

The managed capability ``(MINT id account amount)`` must be installed.

.. code:: lisp

  (mint "t:_yTD6obGrZk07HIEDaZ9pTwKce2Swv4NMN7DhjzjCbI" "r:user.pascal"
        (keyset-ref-guard "user.pacal") 1.0)


burn
~~~~
*id* ``string`` *account* ``string`` *amount* ``decimal`` *→* ``bool``

Burn an *amount* of a token *id* from *account*.


The function calls the hooks ``(enforce-burn)`` from all policies associated with the token.

The managed capability ``(BURN id account amount)`` must be installed.

.. code:: lisp

  (burn "t:_yTD6obGrZk07HIEDaZ9pTwKce2Swv4NMN7DhjzjCbI" "r:user.pascal"
        1.0)


transfer
~~~~~~~~~
*id* ``string`` *sender* ``string`` *receiver* ``string`` *amount* ``decimal`` *→* ``bool``

Transfer an *amount* of a token *id* from *sender* to *receiver*.
The account for this specific token must already exist for this function to work.

The function calls the hooks ``(enforce-transfer)`` from all policies associated with the token.

The managed capability ``(TRANSFER id sender receiver amount)`` must be installed.


.. code:: lisp

  (transfer "t:_yTD6obGrZk07HIEDaZ9pTwKce2Swv4NMN7DhjzjCbI"
            "k:0c1cb582d44802541d905c8b0db25faeabfd6ebf1b20355d9fae1d4de37d590d" "r:user.pascal"
            1.0)


transfer-create
~~~~~~~~~~~~~~~
*id* ``string`` *sender* ``string`` *receiver* ``string`` *receiver-guard* ``guard``  *amount* ``decimal`` *→* ``bool``

Transfer an *amount* of a token *id* from *sender* to *receiver*.
Create the account if it doesn't already exist.


The functions calls the hooks ``(enforce-transfer)`` from all policies associated with the token.

The managed capability ``(TRANSFER id sender receiver amount)`` must be installed.


.. code:: lisp

  (transfer-create "t:_yTD6obGrZk07HIEDaZ9pTwKce2Swv4NMN7DhjzjCbI"
                   "k:0c1cb582d44802541d905c8b0db25faeabfd6ebf1b20355d9fae1d4de37d590d"
                   "r:user.pascal" (keyset-ref-guard "user.pacal")
                   1.0)

sale (defpact)
~~~~~~~~~~~~~~
*id* ``string`` *sellet* ``string`` *amount* ``decimal`` *timeout* ``time`` *→* ``bool``

Put into sale an amount of token, with a given *timeout*.

The exact meaning of *timeout* depends on the policies that handles the sale.

*timeout* can also be ``ledger.NO-TIMEOUT`` to remove any timeout.
Not all policies support ``NO-TIMEOUT``

``sale`` is a pact divided in two steps:

- **Step 0:** Initiate the sale

  - Calls the hooks ``(enforce-offer)`` from all policies associated with the token
  - The capability ``(OFFER id seller amount)`` granted by guard of the seller must have been installed.
  - Some other capabilities may be also required by the policies.
  - The user may supply some objects in the data section of the transactions depending on the policies.

- **Step 0 Rollback**: Cancel the sale.

  - Calls the hooks ``(enforce-withdraw)`` from all policies associated with the token
  - Some capabilities may also be required by the policies.
  - The user may supply some objects in the data section of the transaction depending on the policies.

- **Step 1**: End the sale.

  - Calls the hooks ``(enforce-buy)`` then ``(enforce-settle)`` from all policies associated with the token
  - Some capabilities may also be required by the policies.
  - Two fields are mandatory in the data section of the continuation transaction.

    - **buyer** ``string`` => Buyer of the token
    - **buyer-guard** ``guard`` => Guard of the buyer's account

  - The policies may refuse or accept the buyer.



.. image:: diagrams/defpact_sale_flow.svg

For more details: :ref:`CONCEPTS-SALE`


Events
------
TOKEN-CREATE
~~~~~~~~~~~~
*id* ``string`` *uri* ``string`` *precision* ``integer`` *policies* ``[module{token-policy-ng-v1}]``

Emitted when a token is created.

SUPPLY
~~~~~~
*id* ``string`` *supply* ``decimal``

Emitted when supply is updated


RECONCILE
~~~~~~~~~
*token-id* ``string`` *amount* ``decimal`` *sender* ``object{sender-balance-change}`` *receiver* ``object{receiver-balance-change}``

Event emitted for allowing accounting by events.

``sender-balance-change`` and ``receiver-balance-change`` are defined in the interface.

.. code:: lisp

   (defschema sender-balance-change
     account:string
     previous:decimal
     current:decimal
     )

   (defschema receiver-balance-change
     account:string
     previous:decimal
     current:decimal
     )

- ``account`` may be an empty in case of a burn or mint.
- ``previous`` is the previous balance.
- ``current`` is the new balance.

MINT
~~~~
*id* ``string`` *account* ``string`` *amount* ``decimal``

Event emitted when a token is minted.


BURN
~~~~
*id* ``string`` *account* ``string`` *amount* ``decimal``

Event emitted when a token is burned.

TRANSFER
~~~~~~~~
*id* ``string`` *sender* ``string`` *receiver* ``string`` *amount* ``decimal``

Event emitted when a token is transferred.

*Note:* This event is only emitted following a normal `(transfer)` or `(transfer-create)`.
Not during a sale.

For a better accuracy follow-up of the tokens, you may better use the `RECONCILE` events.

SALE
~~~~
*id* ``string`` *seller* ``string`` *amount* ``decimal`` *timeout* ``time`` *sale-id* ``string``

Event emitted when a sale is initiated.

OFFER
~~~~~
*id* ``string`` *seller* ``string`` *amount* ``decimal``

Event emitted when an offered is being done starting a sale.
