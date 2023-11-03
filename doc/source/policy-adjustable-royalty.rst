.. _POLICY-ADJUSTABLE-ROYALTY:

policy-adjustable-royalty
--------------------------

Description
^^^^^^^^^^^

This policy allows the creator to receive a royalty for each sale.  It's very similar to the standard royalty
policy. The main difference is that the creator is allowed to change the royalty rate at any moment.

The policy have to be initialized at token creation with a receiver account and a royalty fixed rate.

The royalty is paid in the same currency as used during the sale. At creation of the token, the creator must supply
a list of currencies he accepts as payment. Only sales using these currencies will be accepted. The list of allowed currencies
is updatable at later time by the creator.

This policy is triggered during the ``sale-offer`` (by the detection of the common ``sale-msg-sch``) and charged during the ``sale-settle`` just before the seller payment.

When a sale has been started, the rate of the royalty is cached and is fixed until the completion. It means that
a creator can't change the rate of an already existing sale.

Moreover, to prevent front-running from a malicious creator, this policy implements the possibility for a seller to fix a limit of the rate when starting a sale.
This mechanism is very similar to the concept of fixing a minimum output amount when submitting a transaction to a DEX.

The policy works whatever the sale scheme.

*Important note*: It's highly recommended to use :ref:`POLICY-DISABLE-TRANSFER` or :ref:`POLICY-TRUSTED-CUSTODY` in conjunction with this policy.
Otherwise, the royalty could be bypassed by OTC sales.

Implemented hooks
^^^^^^^^^^^^^^^^^

.. code:: lisp

  (defun enforce-init)

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

marmalade_royalty_sale
~~~~~~~~~~~~~~~~~~~~~~
Handled by ``(enforce-sale-offer)``

.. code:: lisp

  (defschema royalty-sale-msg-sch
    maximum_royalty:decimal ; Maximum allowed royalty rate.
  )

This object is optionnal but recommended. It prevents a malicious creator to
frontrun the sale and change the royalty rate at the last moment.


marmalade_royalty
~~~~~~~~~~~~~~~~~~~~~
Handled by ``(enforce-sale-init)``

.. code:: lisp

  (defschema royalty-init-msg-sch
    creator_acct:string ; Creator account: recipient of the royalty
    creator_guard:guard ; Creator account: recipient of the royalty
    rate:decimal ; Royalty rate
    currencies:[module{fungible-v2}] ; List of currencies allowed for royalty payment
  )


This object is mandatory during creation.
``rate`` is between 0.0 and 1.0 (eg. For 5%, rate=0.05)
``currency`` must be the list of allowed currencies for payment.


External functions
^^^^^^^^^^^^^^^^^^
rotate
~~~~~~
*token-id* ``string`` *creator-account* ``string`` *creator-guard* ``guard`` *→* ``string``

Change the destination account and the destination guard of the royalty.
The transaction must be signed by the current guard.

.. code:: lisp

  (use marmalade-ng.policy-adjustable-roayalty)
  (rotate "t:EgYRAWXSd4zZlch3B0cLHTSEt4sgYVg5cwKgvP1CoUs" "r:user.genius-pascal" (create-keyset-ref-guard "user.genius-pascal"))


update-rate
~~~~~~~~~~~
*token-id* ``string`` *new-rate* ``decimal`` *→* ``string``

Change the royalty rate for the given tokenID.

The transaction must be signed by the guard of the creator. The signature can be
scoped by ``(UPDATE-ROYALTY token-id)``

.. code:: lisp

  (use marmalade-ng.policy-adjustable-roayalty)
  (udpate-rate "t:EgYRAWXSd4zZlch3B0cLHTSEt4sgYVg5cwKgvP1CoUs" 0.2)


update-allowed-currencies
~~~~~~~~~~~~~~~~~~~~~~~~~
*token-id* ``string`` *currencies* ``[module{fungible-v2}]`` *→* ``string``

Update the list of currencies accepted by the royalty policy.

The transaction must be signed by the guard of the creator. The signature can be
scoped by ``(UPDATE-ROYALTY token-id)``

.. code:: lisp

  (use marmalade-ng.policy-adjustable-royalty)
  (update-allowed-currencies "t:EgYRAWXSd4zZlch3B0cLHTSEt4sgYVg5cwKgvP1CoUs" [coin free.other-fungible])


View functions
^^^^^^^^^^^^^^
get-royalty-details
~~~~~~~~~~~~~~~~~~~
*token-id* ``string`` *→* ``object{royalty-token-sch}``

Return the details of the royalties for a token.

.. code:: lisp

  (use marmalade-ng.policy-adjustable-roayalty)
  (get-royalty-details "MdXO502ljyF-O6YJV-ODmTuhqFF2Zn6Wa0ONQZu1P8o")

.. code-block::

  {"currencies": [coin],
   "creator-account": "k:9461accc92d3686d075c3147056245eb0098d7a0bec49b669000dab7c5a546ad",
   "creator-guard": KeySet {keys: ["9461accc92d3686d075c3147056245eb0098d7a0bec49b669000dab7c5a546ad"],pred: keys-all},
   "rate": 0.1,
   "token-id": "t:422uU9AJHLeLr6iPGHCkUA_eIvTboKKp5dXaUmctCAw"}


get-royalty-details-per-creator
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
*creator-account* ``string`` *→* ``object{royalty-token-sch}``

Return the details of the royalties for a token for a given creator account.

**Important**: Local only function. Do not use in transactions.

.. code:: lisp

  (use marmalade-ng.policy-adjustable-roayalty)
  (get-royalty-details-per-creator "k:9461accc92d3686d075c3147056245eb0098d7a0bec49b669000dab7c5a546ad")


.. code-block::

  [{"currencies": [coin],
    "creator-account": "k:9461accc92d3686d075c3147056245eb0098d7a0bec49b669000dab7c5a546ad",
    "creator-guard": KeySet {keys: ["9461accc92d3686d075c3147056245eb0098d7a0bec49b669000dab7c5a546ad"],pred: keys-all},
    "rate": 0.1,
    "token-id": "t:422uU9AJHLeLr6iPGHCkUA_eIvTboKKp5dXaUmctCAw"},

   {"currencies": [coin],
    "creator-account": "k:9461accc92d3686d075c3147056245eb0098d7a0bec49b669000dab7c5a546ad",
    "creator-guard": KeySet {keys: ["9461accc92d3686d075c3147056245eb0098d7a0bec49b669000dab7c5a546ad"],pred: keys-all},
    "rate": 0.1,
    "token-id": "t:9Dh2pSjMjXLPERZnbE-aDuXQuquuOkgxSOgS-hYYX7Q"}
  ]

get-sale-rate
~~~~~~~~~~~~~
*sale-id* ``string`` *→* ``string``

Return the royalty rate for a given sale.

Usually the rate is the same as the one returned by `get-royalty-details`.
But if the creator has changed the rate after the sale being started, the old
rate returned by this function is still applied.

.. code:: lisp

  (use marmalade-ng.policy-adjustable-roayalty)
  (get-sale-rate "Lya0Fz-Sl7IuNYp3DOPPtMGU7VFDFZG0mpYd-NneHIs")
    > 0.1

Events
^^^^^^
ROYALTY-PAID
~~~~~~~~~~~~
*token-id* ``string`` *creator-account* ``string`` *amount* ``decimal``

Event emitted when a royalty is paid to a creator.
