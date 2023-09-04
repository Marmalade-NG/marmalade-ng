Module util-policies: Reference and API
=======================================

This module is intended to be only used by policies developers.
It contains some utilities functions to make policies creation more easy
and compliant.

Message utils
-------------
These functions are helpers to read the data messages
following the specification :ref:`DATA-MESSAGES`


get-msg-data
^^^^^^^^^^^^
*domain* ``string``  *token* ``object{token-info}`` *default* ``object``  *→* ``object``

Read an optional message from data, trying to read
- per-token message
- then fallback to a global message
- and if nothing available returns the default as a fallback


enforce-get-msg-data
^^^^^^^^^^^^^^^^^^^^
*domain* ``string``  *token* ``object{token-info}``   *→* ``object``

Similar to **get-msg-data** but without default.

The message is mandatory. Make the transaction fail if not present.

Sales Message utils
-------------------
Marmalade-NG define a common message for sales, shared by all policies.

Definition:

.. code-block:: lisp

  (defschema sale-msg-sch
    sale_type:string ; Type of sale
    currency:module{fungible-v2} ; Currency of sale
  )

Theses functions are helpers to read that specific message:

read-sale-msg
^^^^^^^^^^^^^
*token* ``object{token-info}`` *→* ``object{sale-msg-sch}``
Read the `sale` message returning an empty default if not present.

enforce-read-sale-msg
^^^^^^^^^^^^^^^^^^^^^
*token* ``object{token-info}`` *→* ``object{sale-msg-sch}``

Read the `sale` message, but make the transaction fail if not present.

Other common utils
-------------------

check-fungible-account
^^^^^^^^^^^^^^^^^^^^^^
*currency* ``module{fungible-v2}`` *acct* ``string`` *→* bool

Check (and enforce) an account again a specific fungible currency.
- The account name must be valid.
- The account must exist.

check-price
^^^^^^^^^^^
*currency* ``module{fungible-v2}`` *price* ``decimal`` *→* bool

Check (and enforce) the validity of a price against a specific currency.
- The price must be positive.
- The price must must be compliant with the fungible precision.
