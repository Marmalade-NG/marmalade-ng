For marketplaces : Organizing sales
-----------------------------------

The architecture of Marmalade-NG allow creating a pure de-centralized marketplace,
without the need for a centralized back-end. All needed data is stored on-chain.


Pre-checks
~~~~~~~~~~
Before putting on sales a token, the marketplace should examine closely the list of policies
of the token:

- If the token implements ``DISABLE-SALE`` => Impossible to sell.
- Whether the token implements ``MARKETPLACE``, allowing the marketplace to charge fees.
- Sales mechanisms implemented ``FIXED-SALE`` and/or ``AUCTION-SALE`` and/or ``DUTCH-AUCTION-SALE``.

Depending on the sales mechanisms implemented by the token and by the marketplace itself, the user should
be able to choose how to sell his token.

.. _UNKNOWN_POLICES:

Unknown policies handling
~~~~~~~~~~~~~~~~~~~~~~~~~~
A token may have some custom policies. A custom policy could interfere in many ways during the sale process.
(including causing a loss of funds for users).
If :ref:`STD-POLICIES-POLICIES-TO-LIST` or :ref:`STD-POLICIES-FROM-POLICIES` is used, the unknown policies are referred
as a unique identifier.

Depending on its own policy, the Marketplace can:
  - Ignore unknown policies
  - Whitelist reviewed policies (recommended)
  - Refuse any sales with unknown policies

Guard policy handling
~~~~~~~~~~~~~~~~~~~~~
A token may implement the guard policy. The guard policy is less powerful than a custom
policy, but gives the possibility to create custom and complex solutions.
As a consequence, whether the guard policy will authorize a sale is unpredictable.
The dApp should try it using a local call.

However, as soon as the sale offer has been made, the guard policy can't interfere anymore.

**Thus, a guard policy should be safe for marketplace users**.


Marketplace fee object
~~~~~~~~~~~~~~~~~~~~~~
When the marketplace policy is used for charging a fee, the marketplace must provide a fee object :ref:`POLICY-MARKETPLACE-MARMALADE-MARKETPLACE`
that defines the fee parameters:

- minimal/maximum/rate of the fee
- currency of the fee
- payable account.

Such an object is identified by a hash. The Marketplace may manage one or a set of genuine fee objects (and their corresponding hashes)

This will give the possibility to the marketplace to only list genuine sales.

**Note**: The field ``marketplace-name`` should remain as informative only. Nobody should rely on this field, since it can
be crafted by anyone. Only the full object represented by its hash is reliable.


Fixed quote sales
~~~~~~~~~~~~~~~~~

Starting the sale
^^^^^^^^^^^^^^^^^^
A sale has to be started by invoking the ledger defpact function ``(sale)``.

The timeout parameter can be set to a finite timeout or infinite with the constant `NO-TIMEOUT`.

.. _FIXED-QUOTE-LISTING:

Listing
^^^^^^^
The list of current active sales can be retrieved on-chain by using :ref:`POLICY-FIXED-SALE-GET-ALL-ACTIVE-SALES`.

But if the marketplace applies a strict policy of only displaying the sales that have been created through it.
(or at least the sales that have accepted to comply with the marketplace fees), the function :ref:`POLICY-MARKETPLACE-BY-MARKET-HASH` can be used to
retrieve the list of sales. This function has to be called with the list of known **Genuine hashes**

The marketplace policy is not aware of the type of sale it is. When is sale is retrieved through the marketplace policy module, it should be validated
by the `fixed-sale` module :ref:`POLICY-FIXED-SALE-GET-SALE`.

Moreover, it is recommended to inspect the policies list of the retrieved sale to ensure they are compliant with :ref:`UNKNOWN_POLICES`


Ending the sale
^^^^^^^^^^^^^^^
When a buyer wants to buy the token, the dApp has to call :ref:`POLICY-FIXED-SALE-GET-SALE` to check the conditions:

- price
- currency
- escrow account
- end date

The continuation transaction must be sent with the following parameters:
  - Step = 1
  - Rollback = False


Data:

.. code::

  {"buyer": buyer,
   "buyer-guard": buyer-guard
  }

And the following installed capacity:

.. code:: lisp

    (currency.TRANSFER buyer escrow-account price)


Auction  sale
~~~~~~~~~~~~~

Starting the sale
^^^^^^^^^^^^^^^^^
A sale has to be started by invoking the ledger defpact function ``(sale)``.

The timeout parameter must be a finite timeout.

Listing
^^^^^^^
The listing procedures are similar to :ref:`FIXED-QUOTE-LISTING`, but the ``policy-auction-same`` policy module has to be used
instead of the ``fixed-auction-sale``:

- :ref:`POLICY-AUCTION-SALE-GET-ALL-ACTIVE-SALES` and :ref:`POLICY-AUCTION-SALE-GET-SALE`

Bidding
^^^^^^^
When a buyer wants to bid for a token, the dApp has to call :ref:`POLICY-AUCTION-SALE-GET-SALE` to check the conditions:

- current-price / start-price / increment
- currency
- escrow account
- end date

The minimum price should be calculated by the dApp with the following algorithm:

.. code::

  IF current-price = 0
    THEN
      minimum-price = starting-price
    ELSE
      minimum-price = current-price * increment
  ENDIF

**Note**: The user is able to bid more than the minimum price.

The bid must be done using the function :ref:`POLICY-AUCTION-SALE-PLACE-BID` of the `policy-auction-sale` module.


And the following installed capacity:

.. code:: lisp

    (currency.TRANSFER buyer escrow-account new-price)


Ending the sale
^^^^^^^^^^^^^^^
The continuation transaction can be triggered by:
  - the seller
  - the buyer
  - or anybody else, including a bot that works for the marketplace.

It is necessary to retrieve the following parameters using :ref:`POLICY-AUCTION-SALE-GET-SALE`:
  - buyer
  - end-date

Ending the sale is only possible if ``end-date`` is earlier than the current date-time.

Depending on the value of the ``buyer`` field, one of the two procedures must be done:

Withdrawal (``buyer`` = "")
...........................
It means that nobody has proposed a bid.

A defpact continuation transaction must be sent with:
  - Step = 0
  - Rollback = True

Nothing more is needed. The tokens will be sent back to the seller.

Settle transaction (``buyer`` not = "")
.......................................
The buyer must be retrieved with :ref:`POLICY-AUCTION-SALE-GET-SALE`.

The buyer guard must be retrieved with :ref:`LEDGER-TOTAL-ACCOUNT-GUARD`.
The dApp shouldn't try to infer the guard for the account name (e.g: Extracting the key from a k:account name).

A defpact continuation transaction must be sent with:
  - Step = 1
  - Rollback = False

Data:

.. code::

  {"buyer": buyer,
   "buyer-guard": buyer-guard
  }
