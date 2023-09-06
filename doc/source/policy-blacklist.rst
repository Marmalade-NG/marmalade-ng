.. _POLICY-BLACKLIST:

policy-blacklist
----------------

Description
^^^^^^^^^^^

This policy creates a blacklist feature. It allows a token creator/owner
to blacklist accounts. The blacklisted accounts are frozen and any further usage of then token is prevented.

The blacklisting is done per token basis.

To activate this feature, the field ``marmalade_blacklist`` or ``marmalade_blacklist_tokenid`` must be present during the token creation.


Implemented hooks
^^^^^^^^^^^^^^^^^

.. code:: lisp

  (defun enforce-init)

  (defun enforce-mint)

  (defun enforce-burn)

  (defun enforce-transfer)

  (defun enforce-sale-offer)



Input data structures
^^^^^^^^^^^^^^^^^^^^^

marmalade_blacklist
~~~~~~~~~~~~~~~~~~~~~~~

Handled by ``(create-token)``

.. code:: lisp

  (defschema blacklist-msg-sch
    guard:guard ; Admnistrative guard for blacklisting
  )



External functions
^^^^^^^^^^^^^^^^^^

add-account
~~~~~~~~~~~
*token-id* ``string`` *account* ``string`` *→* ``string``

Add an account in the blacklist for a given token.

The transaction must be signed by the guard given during token creation.

The signature can be scoped by ``(UPDATE-BLACKLIST token-id account true)``

.. code:: lisp

  (use marmalade-ng.policy-blacklist)
  (add-account "t:EgYRAWXSd4zZlch3B0cLHTSEt4sgYVg5cwKgvP1CoUs" "k:2e04bc04a9cf96701a806110242aee08a1692437413bead299fffb4a5b2e4bb6")




remove-account
~~~~~~~~~~~~~~
*token-id* ``string`` *account* ``string`` *→* ``string``

Remove an account from the blacklist for a given token.

The transaction must be signed by the guard given during token creation.

The signature can be scoped by ``(UPDATE-BLACKLIST token-id account false)``

.. code:: lisp

  (use marmalade-ng.policy-blacklist)
  (remove-account "t:EgYRAWXSd4zZlch3B0cLHTSEt4sgYVg5cwKgvP1CoUs" "k:2e04bc04a9cf96701a806110242aee08a1692437413bead299fffb4a5b2e4bb6")



View functions
^^^^^^^^^^^^^^
is-blacklisted
~~~~~~~~~~~~~~
*token-id* ``string`` *account* ``string`` *→* ``bool``

Return true if the account is blacklisted for the given token.

.. code:: lisp

  (use marmalade-ng.policy-blacklist)
  (is-blacklisted "t:EgYRAWXSd4zZlch3B0cLHTSEt4sgYVg5cwKgvP1CoUs" "k:2e04bc04a9cf96701a806110242aee08a1692437413bead299fffb4a5b2e4bb6")
    > true

list-blacklisted-accounts
~~~~~~~~~~~~~~~~~~~~~~~~~
*token-id* ``string`` *→* ``[string]``

Return the list of blacklisted accounts for a specific token.

**Important**: Local only function. Do not use in transactions

.. code:: lisp

  (use marmalade-ng.policy-blacklist)
  (list-blacklisted-accounts "t:EgYRAWXSd4zZlch3B0cLHTSEt4sgYVg5cwKgvP1CoUs")
    > ["k:2e04bc04a9cf96701a806110242aee08a1692437413bead299fffb4a5b2e4bb6",
       "k:5b5428f60670411fa6dde1100b9f9a1ce9b276a9b51caeac063573936af4b0d7"]

list-blacklisted-tokens
~~~~~~~~~~~~~~~~~~~~~~~
*account* ``string`` *→* ``[string]``

Return the list of blacklisted tokens for a specific account.

**Important**: Local only function. Do not use in transactions

.. code:: lisp

  (use marmalade-ng.policy-blacklist)
  (list-blacklisted-tokens "k:2e04bc04a9cf96701a806110242aee08a1692437413bead299fffb4a5b2e4bb6")
    > ["t:EgYRAWXSd4zZlch3B0cLHTSEt4sgYVg5cwKgvP1CoUs"]
