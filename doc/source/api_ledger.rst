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

Events
------
