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

.. _POLICY-MARKETPLACE-SHARED-FEES:

Shared fees
~~~~~~~~~~~
The marketplace that created the sale has the possibility of sharing its fees with the buying marketplace.

The following conditions must be met:
  - The ``shared-rate`` field in the fee object must be set (between 0.0 and 1.0)
  - During the continue transaction the buying marketplace must include the ``shared_fee`` object in the transaction data.

Then, the fees are split in the following way:
   - Buying Marketplace: Buying_Fee = shared-rate * Fee
   - Selling Marketplace : Selling_Fee = (1.0 - shared-rate) * Fee
   - If there is no ``shared-fee`` object in the continue transaction, the Buying marketplace receives the totality of the fee.

A Marketplace that doesn't want to share fees must simply include shared-rate = 0.0 in the fee object.

**Note:** A smart user might buy directly the NFT without using a Marketplace Frontend. In this case, the user
will be able to appropriate the shared part of the marketplace fee.


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
    shared-rate:decimal; Share fee with the buying marketplace
  )

Both objects must be present in the ``(sale)`` transaction. And both objects must
have the same currency.

marmalade_shared_fee
~~~~~~~~~~~~~~~~~~~~
Handled by ``(enforce-sale-settle)``

Optional object. If not present, the whole fee amount is paid to the initial market place.

.. code:: lisp

  (defschema shared-fee-msg
    recipient:string ; Recipient account for the shared-fee
  )


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
                       "min-fee": 0.2,
                       "shared-rate":0.0},
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
                       "min-fee": 0.0,
                       "shared-rate":0.0},
    "marketplace-hash": "zE-T8f_kTazOs7IuC-dNZ4Nf3KnkDymeozRb66QlrBk",
    "sale-id": "rmIkCdd9907zaaVDRhnkIiig1mZclYnkLbsGzgXuCLk",
    "token-id": "t:9Dh2pSjMjXLPERZnbE-aDuXQuquuOkgxSOgS-hYYX7Q"},

    {"enabled": true,
     "marketplace-fee": {"currency": coin,
                         "fee-rate": 0.1,
                         "marketplace-account": "r.user.best-market",
                         "marketplace-name": "BestMarket",
                         "max-fee": 10000.0,
                         "min-fee": 0.2,
                         "shared-rate":0.0},
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
                       "min-fee": 0.0,
                       "shared-rate":0.0},
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
