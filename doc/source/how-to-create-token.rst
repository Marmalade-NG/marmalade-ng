For tokens creators
-------------------

This cookbook is just an example of how it can be done. But there are multiple
other possibilities.

Choose a set of policies
~~~~~~~~~~~~~~~~~~~~~~~~
There is no rule of thumb...

However some guidelines:

Choose at least one policy that manaages the minting process:
   - ``INSTANT-MINT`` or ``GUARDS``
   - ``FIXED-ISSUANCE``
   - ``NON-FUNGIBLE``

If you want your token being part of a collection, choose ``COLLECTION``

Choose at least one of these policy to manage sales:
   - ``DISABLE-SALE``
   - ``FIXED-SALE``
   - ``AUCTION-SALE``
   - ``DUTCH-AUCTION-SALE``

*Note*: Several sales policies can live together and will be triggered dynamically
according to the choice of the seller

If you have selected one sale policy, you may want to use ``MARKETPLACE`` to allow the token
to be sold on a Marketplace.

As well, you may want to choose ``ROYALTY`` or ``ADJUSTABLE-ROYALTY``.

Usually ``EXTRA-POLICY`` should be chosen to benefit from the new features and new sales
mechanisms introduced by the devellopers.


Optional: Create a collection
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
If your tokens will be part of a collection (and you will use the ``COLLECTION`` policy),
you have to create a collection before creating your tokens.

You need a keypair to protect your collection.

Generate a collection-id
^^^^^^^^^^^^^^^^^^^^^^^^
We need to create off-line (using a local call to a node) a collection-id.

.. code-block:: lisp
  :caption: Pact code

  (use marmalade-ng.policy-collection)
  (create-collection-id "RedTulips" (read-keyset 'ks))

.. code-block:: json
  :caption: Transaction data

  {"ks":{"pred":"keys-all",
         "keys":["collection-key"]}}

Create the collection
^^^^^^^^^^^^^^^^^^^^^
Send a transaction to create the collection using the collection-id you just get from the last step.
Sign the transaction with "collection-key".

.. code-block:: lisp
  :caption: Pact code

  (use marmalade-ng.policy-collection)
  (create-collection:bool ("c_RedTulips_ZMLLJuSq0JoHSR4f_ZgUa2H_p7Rr71CN8CjQ7ZL_hU0"
                           "RedTulips" 0 (read-keyset 'ks)))

.. code-block:: json
  :caption: Transaction data

  {"ks":{"pred":"keys-all",
         "keys":["collection-key"]}}

Create the token
~~~~~~~~~~~~~~~~
A temporary keypair is needed to protect the token creation.

Another key will be used to protect minting.

In this example, we generate a token with the following policies:
    - COLLECTION
    - INSTANT-MINT
    - DISABLE-TRANSFER
    - FIXED-SALE
    - ROYALTY
    - NON-FUNGIBLE

Generate a token-id
^^^^^^^^^^^^^^^^^^^
We need to create off-line (using a local call to a node) a token-id.

.. code-block:: lisp
  :caption: Pact code

  (use marmalade-ng.ledger)
  (create-token-id  (read-keyset 'c-ks) "https://red-tulips.com/tulip-1")

.. code-block:: json
  :caption: Transaction data

  {"c-ks":{"pred":"keys-all",
         "keys":["creation-key"]}}


Create and mint the token
^^^^^^^^^^^^^^^^^^^^^^^^^

Since we use the policy **INSTANT-MINT**, the token will be minted in the same transaction

To create the policy list, we can use the ``std-policies`` module, which provides some helpers.

We need to include the objects ``marmalade_collection`` and ``marmalade_royalty`` in the data section of the transaction.


.. code-block:: lisp
  :caption: Pact code

  (use marmalade-ng.ledger)
  (use marmalade-ng.std-policies)
  (create-token "t:QvuWzPPKhSlueC9hryQKJ-ItFPGtdOhDDhrD4q8lc-I" 0
                 "https://red-tulips.com/tulip-1"
                 (to-policies "COLLECTION INSTANT-MINT DISABLE-TRANSFER FIXED-SALE ROYALTY NON-FUNGIBLE")
                 (read-keyset 'c-ks))

  (mint "t:QvuWzPPKhSlueC9hryQKJ-ItFPGtdOhDDhrD4q8lc-I"
         "k:fc87dec81d12e2e49e50eea2bf10662952a0435d29f7eed473356a3f23828559"
         (read-keyset 'ks-first-owner)
         1.0)


.. code-block:: json
   :caption: Transaction data

   {"c-ks":{"pred":"keys-all",
            "keys":["creation-key"]},

    "marmalade_collection":{"id": "c_RedTulips_ZMLLJuSq0JoHSR4f_ZgUa2H_p7Rr71CN8CjQ7ZL_hU0" },

    "ks-first-owner": {"pred":"keys-all",
                       "keys":["fc87dec81d12e2e49e50eea2bf10662952a0435d29f7eed473356a3f23828559"]},

    "marmalade_royalty": {"creator_acct":"k:9ded186eb20c495ca1f08d59722237024282da264db1ed8d5aaf4ca4d351edd0",
                          "creator_guard":{"pred":"keys-all",
                                           "keys":["9ded186eb20c495ca1f08d59722237024282da264db1ed8d5aaf4ca4d351edd0"]},
                          "rate": 0.05 }
    }

- Sign with ``creation-key`` (cap ``(ledger.ENFORCE-RESERVED)``)
- Sign with ``collection-key`` (cap ``(policy-collection.ADD-TO-COLLECTION "c_RedTulips_ZMLLJuSq0JoHSR4f_ZgUa2H_p7Rr71CN8CjQ7ZL_hU0" "t:QvuWzPPKhSlueC9hryQKJ-ItFPGtdOhDDhrD4q8lc-I")``)
- Sign with any key the managed cap ``(ledger.MINT "t:QvuWzPPKhSlueC9hryQKJ-ItFPGtdOhDDhrD4q8lc-I" "k:fc87dec81d12e2e49e50eea2bf10662952a0435d29f7eed473356a3f23828559" 1.0)``
