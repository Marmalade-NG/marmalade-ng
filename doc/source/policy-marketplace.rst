.. _POLICY-MARKETPLACE:

policy-marketplace
------------------

Description
^^^^^^^^^^^

This policy allows marketplaces to charge fees during a sale.

This policy is triggered during the ```sale-offer```. The field
``marmalade_marketplace`` or ``marmalade_marketplace_tokenid`` must be included in the transaction.

Three value are considered:
  - The minimum absolute fee: Can be 0.0
  - The maximum absolute fee
  - The fee rate

Then the marketplace fee is calculated with the following formula:

  Fee = MAX( MIN ( Sale_Price * Rate, Max_Fee) , Min_Fee)

When a new sale is created and a fee object submitted, the module computes the Hash of the fee object.

This hash let the possibility to the market place to determine whether the fee object is genuine and decide
whether this sale can be listed or not.

This policy works whatever the sale scheme.

Implemented hooks
^^^^^^^^^^^^^^^^^

.. code:: lisp

  (defun enforce-sale-offer)

  (defun enforce-sale-settle)


Input data structures
^^^^^^^^^^^^^^^^^^^^^

marmalade_sale
~~~~~~~~~~~~~~
Handled by ``(enforce-sale-offer)``

.. code:: lisp

  (defschema sale-msg-sch
    sale_type:string ; Type of sale
    currency:module{fungible-v2} ; Currency of sale
  )

.. _POLICY-MARKETPLACE-MARMALADE-MARKETPLACE:

marmalade_marketplace
~~~~~~~~~~~~~~~~~~~~~
Handled by ``(enforce-sale-offer)``

.. code:: lisp

  (defschema marketplace-fee-sch
    marketplace-name:string ; Name of the marketplace : Informative only
    marketplace-account:string ; Marketplace account : where the funds must be sent
    currency:module{fungible-v2} ; Currency : must be the same as the sale structure
    min-fee:decimal ; Minimum absolute fee
    fee-rate:decimal ; Fee rate
    max-fee:decimal ; Maximum absolute fee
  )

Both objects must be present in the ``(sale)`` transaction. And both objects must
have the same currency.



External functions
^^^^^^^^^^^^^^^^^^
Nope

View functions
^^^^^^^^^^^^^^
get-marketplace-fee
~~~~~~~~~~~~~~~~~~~~
*sale-id* ``string`` *→* ``object{marketplace-sale-sch}``

Return the detail of the marketplace fee record for the given sale

.. code:: lisp

  (use marmalade-ng.policy-marketplace)
  (get-marketplace-fee "MdXO502ljyF-O6YJV-ODmTuhqFF2Zn6Wa0ONQZu1P8o")

.. code-block::

  {"enabled": true,
   "marketplace-fee": {"currency": coin,
                       "fee-rate": 0.1,
                       "marketplace-account": "r.user.best-market",
                       "marketplace-name": "BestMarket",
                       "max-fee": 10000.0,
                       "min-fee": 0.2},
   "marketplace-hash": "clALAwFdf6Xd17bVFGK1Jxo6b92TkNdZ2YHD4I3ZtKw",
   "sale-id": "MdXO502ljyF-O6YJV-ODmTuhqFF2Zn6Wa0ONQZu1P8o",
   "token-id": "t:QvuWzPPKhSlueC9hryQKJ-ItFPGtdOhDDhrD4q8lc-I"}


get-active-sales-by-name
~~~~~~~~~~~~~~~~~~~~~~~~~
*market-name* ``string`` *→* ``object{marketplace-sale-sch}``

Return the details of the market place fee record for a given market place name.

A marketplace should not rely blindly on these information. The marketplace must check each record for a known hash.

**Important**: Local only function. Do not use in transactions

.. code:: lisp

  (use marmalade-ng.policy-marketplace)
  (get-active-sales-by-name "BestMarket")


.. code-block::

  [{"enabled": true,
    "marketplace-fee": {"currency": coin,
                       "fee-rate": 0.1,
                       "marketplace-account": "r.user.best-market",
                       "marketplace-name": "BestMarket",
                       "max-fee": 0.2,
                       "min-fee": 0.0},
    "marketplace-hash": "zE-T8f_kTazOs7IuC-dNZ4Nf3KnkDymeozRb66QlrBk",
    "sale-id": "rmIkCdd9907zaaVDRhnkIiig1mZclYnkLbsGzgXuCLk",
    "token-id": "t:9Dh2pSjMjXLPERZnbE-aDuXQuquuOkgxSOgS-hYYX7Q"},

    {"enabled": true,
     "marketplace-fee": {"currency": coin,
                         "fee-rate": 0.1,
                         "marketplace-account": "r.user.best-market",
                         "marketplace-name": "BestMarket",
                         "max-fee": 10000.0,
                         "min-fee": 0.2},
     "marketplace-hash": "clALAwFdf6Xd17bVFGK1Jxo6b92TkNdZ2YHD4I3ZtKw",
     "sale-id": "MdXO502ljyF-O6YJV-ODmTuhqFF2Zn6Wa0ONQZu1P8o",
     "token-id": "t:QvuWzPPKhSlueC9hryQKJ-ItFPGtdOhDDhrD4q8lc-I"}
   ]

.. _POLICY-MARKETPLACE-BY-MARKET-HASH:

get-active-sales-by-market-hash
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
*market-hash* ``string`` *→* ``[object{marketplace-sale-sch}]``

Return the details of the market place fee record for a given market hash.

**Important**: Local only function. Do not use in transactions

.. code:: lisp

  (use marmalade-ng.policy-marketplace)
  (get-active-sales-by-hash "clALAwFdf6Xd17bVFGK1Jxo6b92TkNdZ2YHD4I3ZtKw")

.. code-block::

  [{"enabled": true,
    "marketplace-fee": {"currency": coin,
                       "fee-rate": 0.1,
                       "marketplace-account": "r.user.best-market",
                       "marketplace-name": "BestMarket",
                       "max-fee": 0.2,
                       "min-fee": 0.0},
    "marketplace-hash": "zE-T8f_kTazOs7IuC-dNZ4Nf3KnkDymeozRb66QlrBk",
    "sale-id": "rmIkCdd9907zaaVDRhnkIiig1mZclYnkLbsGzgXuCLk",
    "token-id": "t:9Dh2pSjMjXLPERZnbE-aDuXQuquuOkgxSOgS-hYYX7Q"}
   ]


Events
^^^^^^
MARKETPLACE-PAID
~~~~~~~~~~~~~~~~
*token-id* ``string`` *marketplace-account* ``string`` *marketplace-hash* ``string`` *amount* ``decimal``

Event emitted when a fee is paid to the marketplace.
