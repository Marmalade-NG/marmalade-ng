Wallets
-----------

Retrieving the list of tokens owned by an account
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The list of tokens can simply be retrieved from the ledger using the `list-balances` function.

.. code-block:: lisp

  (use marmalade-ng.ledger)
  (list-balances "k:a2eef345a1084d4cd095d172d429152767d30f496d9af30ad54922fdca1b3a76")


.. code-block::

    [{"balance":1,"id":"t:3zJGo9-A-3CMWVqRsD87IIjjP4RauOoFl_TNBfUMg_s"},
     {"balance":1,"id":"t:gvCLwzNLVtZzF2KHEQjUb3pEpyz0iCsXb8ovzfIrPrU"},
     {"balance":1,"id":"t:rJsiA4_qhpNtKgsPtjDzxRo6pvGIB21EMHRLmatjyDg"}]

Retrieving the list of tokens being currently sold by an account
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In Marmalade-NG, when being put in sale, tokens are transferred to escrow accounts.

As consequence, they don't appear anymore in the results given by ``(list-balances)``.

The wallet could retrieve these "hidden" tokens using the ``(get-sales-from-account)`` function
of the sales policies:
:ref:`POLICY-FIXED-SALE-GET-SALES-FROM-ACCOUNT`, :ref:`POLICY-AUCTION-SALE-GET-SALES-FROM-ACCOUNT`, :ref:`POLICY-DUTCH-AUCTION-SALE-GET-SALES-FROM-ACCOUNT`

.. code-block:: lisp

  (free.util-lists.chain [
    (marmalade-ng.policy-fixed-sale.get-sales-from-account "k:a2eef345a1084d4cd095d172d429152767d30f496d9af30ad54922fdca1b3a76"),
    (marmalade-ng.policy-auction-sale.get-sales-from-account "k:a2eef345a1084d4cd095d172d429152767d30f496d9af30ad54922fdca1b3a76"),
    (marmalade-ng.policy-dutch-auction-sale.get-sales-from-account "k:a2eef345a1084d4cd095d172d429152767d30f496d9af30ad54922fdca1b3a76")
  ])


Retrieve information about a token
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

General information
^^^^^^^^^^^^^^^^^^^
 - Retrieve the URI: :ref:`LEDGER-GET-URI`. Necessary to retrieve the metadata of the token.
 - Retrieve the total-supply: :ref:`LEDGER-TOTAL-SUPPLY`. The share of the user can be computed by **balance/total-supply**.
 - Retrieve the list of policies :ref:`LEDGER-GET-POLICIES`. The policies list may be post-processed using:  :ref:`STD-POLICIES-POLICIES-TO-LIST`.

Example:

.. code-block:: lisp

  (use marmalade-ng.ledger)
  (use marmalade-ng.std-policies)

  [
    (get-uri "t:3zJGo9-A-3CMWVqRsD87IIjjP4RauOoFl_TNBfUMg_s")
    (total-supply "t:3zJGo9-A-3CMWVqRsD87IIjjP4RauOoFl_TNBfUMg_s")
    (policies-to-list (get-policies "t:3zJGo9-A-3CMWVqRsD87IIjjP4RauOoFl_TNBfUMg_s"))
  ]


.. code::

  ["http://cat-4",
   1,
   ["COLLECTION","NON-FUNGIBLE","INSTANT-MINT","DISABLE-TRANSFER","ROYALTY","FIXED-SALE","AUCTION-SALE"]
  ]

The wallet should clearly display the list of policies.



Policies specific information
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Depending on the policies implemented by the token, the user may be interested in some other data:

If the token implements ``COLLECTION``:
  - The collection data may be retrieved with :ref:`POLICY-COLLECTION-GET-TOKEN-COLLECTION`

If the token implements ``ROYALTY`` or ``ADJUSTABLE-ROYALTY``:
  - The royalty data may be retrieved with :ref:`POLICY-ROYALTY-GET-ROYALTY-DETAILS`

If the token implements ``FIXED-ISSUANCE``:
  - The issuance specification (especially the `max-supply`) may be retrieved with :ref:`POLICY-FIXED-ISSUANCE-GET-ISSUANCE-SPEC`




Transfer a token (or a part of a token) to another account
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

To transfer a token, the wallet must verify that the token does not implement the ``DISABLE-TRANSFER`` policy.
Note that some other policies can prevent the transfer (especially ``GUARDS``, or custom policies).
Predicting whether a token can be transferred is not always possible. Thus, the easiest and most recommended way is to always try the transaction in local.

The wallet must retrieve the token's precision to validate the amount transferred.
A special (but usual) case is when the token implements the ``NON-FUNGIBLE`` policy: precision = 1 and the token is only transferable as a whole.

The ``(ledger.TRANSFER id sender receiver amount)`` capability must be installed by scoping the signature.

And the ledger `transfer` or `transfer-create` function can be called:

.. code-block:: lisp

  (marmalade-ng.ledger.transfer "t:3zJGo9-A-3CMWVqRsD87IIjjP4RauOoFl_TNBfUMg_s",
                                "k:a2eef345a1084d4cd095d172d429152767d30f496d9af30ad54922fdca1b3a76",
                                "k:6420be9176aedacdf44bbc186e63ac8d3ac8dd0fe53f711c0e589b98ed5dd59c"
                                1.0)
