.. _POLICY-AUCTION-SALE:

policy-auction-sale
-------------------

Description
^^^^^^^^^^^

This policy manages an auction sale:
  - The seller of the token puts its token on sale and propose a starting price and a multiplier increment.
  - People proposes bids:
    - starting with at least the starting price
    - then by bidding a price that it is at least the last price multiplied by the increment.

Since the `(place-bid)` function is executed out of the defpact context, a second escrow account must be used during
the auctions. The funds from this second escrow account are then transferred to the main Marmalade escrow account before settlement.


This policy work in several steps:

1 - The seller call the defpact ``sale``. The policy is triggered when ``sale_type`` = "auction".
The policy record the minimum price and the increment.

2 - Buyers call `(place-bid)` and transfers the amount of their bids to the escrow account.

2 - Once the timeout has elapsed, the seller, the buyer or anybody else complete the defpact. The dale amount is transferred to the main escrow account.

In the mean time, some policies with an higher priority (like marketplace or royalty) are allowed
to take a part of the funds from the escrow account.

3 - The ``(enforce-sale-settle)`` hook is called, and the seller is paid with the remaining funds.

The seller is able to withdraw from the sale after the ``timeout has elapsed`` is nobody has placed a bid


This policy does not support the ``NO-TIMEOUT`` parameter.

Implemented hooks
^^^^^^^^^^^^^^^^^

.. code:: lisp

  (defun enforce-sale-offer)

  (defun enforce-sale-withdraw)

  (defun enforce-sale-buy)

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

Mandatory to trigger the policy with ``sale_type`` = auction

auction
~~~~~~~
Handled by ``(enforce-sale-offer)``

.. code:: lisp

  (defschema auction-msg-sch
    start_price:decimal ; start price of the auction
    recipient:string ; recipient account
    increment_ratio:decimal ;increment ratio (multiplier) between each bid
  )

Mandatory


External functions
^^^^^^^^^^^^^^^^^^
place-bid
~~~~~~~~~
*sale-id* ``string` *buyer* ``string`` *buyer-guard* ``guard`` *new-price* ``decimal``  *→* ``string``

Place a new bid on the given sale ID.

The bidder should before call ``(get-sale)`` to check the current state of the sale:

- current-price
- escrow account
- increment

The bidder must have installed the TRANSFER capability of the payment currency.

The previous best buyer is automatically refunded

.. code:: lisp

  (use marmalade-ng.policy-auction-sale)
  (place-bid "utRLLLbDcGyMnBcuPDjo-VaxR_65Wxv2v8EhoD9z4-E" "r:user.rich-buyer"
             (keyset-ref-guard "user.rich-buyer") 25.0)




External request functions
^^^^^^^^^^^^^^^^^^^^^^^^^^
get-sale
~~~~~~~~
*sale-id* ``string`` *→* ``object{quote-sch}``

Return the sale details of a given sale-id.

.. code:: lisp

  (use marmalade-ng.policy-auction-sale)
  (get-sale "utRLLLbDcGyMnBcuPDjo-VaxR_65Wxv2v8EhoD9z4-E")

.. code-block::

  {"amount": 0.1,
   "currency": coin,
   "current-buyer": "",
   "current-price": 0.0,
   "enabled": true,
   "escrow-account": "c:09-juxzBu412pfsgBlM6Au7fOnwvSsX78Er66vnC6sI",
   "increment-ratio": 1.1,
   "recipient": "alice",
   "sale-id": "",
   "start-price": 10.0,
   "timeout": "2023-06-01T00:00:00Z",
   "token-id": "t:LWZdYIxjht_J_PCA4RrThTdjD9VDCvkWabnh8tKNST8"
  }


get-sales-for-token
~~~~~~~~~~~~~~~~~~~
*token-id* ``string`` *→* ``[object{quote-sch}]``

Return all the active sales details for a given token.

**Important**: Local only function. Do not use in transactions

.. code:: lisp

  (use marmalade-ng.policy-auction-sale)
  (get-sales-for-token "t:LWZdYIxjht_J_PCA4RrThTdjD9VDCvkWabnh8tKNST8")

.. code-block::

  [{"amount": 0.1,
     "currency": coin,
     "current-buyer": "",
     "current-price": 0.0,
     "enabled": true,
     "escrow-account": "c:09-juxzBu412pfsgBlM6Au7fOnwvSsX78Er66vnC6sI",
     "increment-ratio": 1.1,
     "recipient": "alice",
     "sale-id": "",
     "start-price": 10.0,
     "timeout": "2023-06-01T00:00:00Z",
     "token-id": "t:LWZdYIxjht_J_PCA4RrThTdjD9VDCvkWabnh8tKNST8"
    }]

get-all-active-sales
~~~~~~~~~~~~~~~~~~~~
*→* ``[object{quote-sch}]``

Return all the active sales details.

**Important**: Local only function. Do not use in transactions


.. code:: lisp

  (use marmalade-ng.policy-auction-sale)
  (get-all-active-sales)

.. code-block::

  [{"amount": 0.1,
     "currency": coin,
     "current-buyer": "",
     "current-price": 0.0,
     "enabled": true,
     "escrow-account": "c:09-juxzBu412pfsgBlM6Au7fOnwvSsX78Er66vnC6sI",
     "increment-ratio": 1.1,
     "recipient": "alice",
     "sale-id": "",
     "start-price": 10.0,
     "timeout": "2023-06-01T00:00:00Z",
     "token-id": "t:LWZdYIxjht_J_PCA4RrThTdjD9VDCvkWabnh8tKNST8"
    },

   {"amount": 0.5,
    "currency": coin,
    "current-buyer": "",
    "current-price": 0.0,
    "enabled": true,
    "escrow-account": "c:_xRzg2I2WJu1hov-d8HmuKGq0Kxr5hndunDnkbFpu0Q",
    "increment-ratio": 1.1,
    "recipient": "alice",
    "sale-id": "DmxMgittQd4Duf0WQdeySZkx_I4yvQ3phecLpWMzmw8",
    "start-price": 10.0,
    "timeout": "2023-06-01T00:00:00Z",
    "token-id": "t:sMd0A3s6ZoiHd0RCzZ3XqVcTmOcoNvi73hl1gWXUMSA"}
  ]