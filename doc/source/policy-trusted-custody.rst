.. _POLICY-TRUSTED-CUSTODY:

policy-trusted-custody
----------------------

Description
^^^^^^^^^^^

This policy allows the creator to declare a list of trusted custodians.

- NFT lending protocols (which use NFTs as collateral, for example)
- Trusted custody marketplaces (which pay royalties by other means for example)
- Special contracts relative to these tokens (special minting accounts)
- ...

When a custodial is declared, transfers from and to it are allowed, and can bypass
royalties payment.

Whitelisted custodians can be directly account names or prefixes. This allows to match a
list of user (``u:``) or module (``m:``) guarded accounts.
For example, it's possible to declare ``u:trusted-ns:lending-contract``. This will match
to all "user guards principals" of the `lending-contract`.

To prevent the creator from removing whitelisted custodians and blocking NFTs the policy
works in the following way:

- The policy manages an output allowance database per (token / custodial account) tuple.
- When the recipient of the transfer is whitelisted, the transfer is allowed, and the outbound allowance of the custodial
  is incremented.
- When the sender of the transfer owns allowance, the transfer is allowed, and the outbound allowance of the custodial is decremented.

This guarantees that once the token has been deposited to a custodian account, withdrawal will always be possible.

Intentionally, the outbound balance will be accounted:

- At transfers
- At mint (allowing to mint a collection to a special initial account)
- But **NOT** at burn and sales. (burning or selling won't decrement the outbound balance, and receiving
  a token by sale won't increment the outbound balance)



The policy is incompatible with :ref:`POLICY-DISABLE-TRANSFER`.



Implemented hooks
^^^^^^^^^^^^^^^^^

.. code:: lisp

  (defun enforce-init)

  (defun enforce-transfer)

  (defun enforce-mint)


Input data structures
^^^^^^^^^^^^^^^^^^^^^

marmalade_trusted_custody
~~~~~~~~~~~~~~~~~~~~~~~~~
Handled by ``(enforce-init)``

.. code:: lisp

  (defschema trusted-custody-msg
    guard:guard ; Owned by the creator: used to protect the custodian list
    custodians:[string] ; List of allowed custodians prefixes
  )


External functions
^^^^^^^^^^^^^^^^^^
add-custodian
~~~~~~~~~~~~~~
*token-id* ``string`` *custodian-prefix* ``string`` *→* ``string``

Add an allowed custodian prefix to the given *token-id*.

.. code:: lisp

  (use marmalade-ng.policy-trusted-policy)
  (add-custodian "t:EgYRAWXSd4zZlch3B0cLHTSEt4sgYVg5cwKgvP1CoUs" "u:my-ns.trusted-module")


remove-custodian
~~~~~~~~~~~~~~~~~
*token-id* ``string`` *custodian-prefix* ``string`` *→* ``string``

Remove a previously allowed custodian prefix from the given *token-id*.

.. code:: lisp

  (use marmalade-ng.policy-trusted-policy)
  (remove-custodian "t:EgYRAWXSd4zZlch3B0cLHTSEt4sgYVg5cwKgvP1CoUs" "u:my-ns.trusted-module")



View functions
^^^^^^^^^^^^^^
get-custodians-list
~~~~~~~~~~~~~~~~~~~
*token-id* ``string`` *→* ``[string]``

Return the list of allowed custodian prefixes for the given *token-id*.

.. code:: lisp

  (use marmalade-ng.policy-trusted-policy)
  (get-custodian-list "t:EgYRAWXSd4zZlch3B0cLHTSEt4sgYVg5cwKgvP1CoUs")
    > ["u:my-ns.trusted-module", "u:my-ns.other-trusted-mod"]


custodian-balance
~~~~~~~~~~~~~~~~~
*token-id* ``string`` *custodian-account* ``string`` *→* ``decimal``

Return the outbound balance of a custiodian account for a given  *token-id*.

.. code:: lisp

  (use marmalade-ng.policy-trusted-policy)
  (custodian-balance "t:EgYRAWXSd4zZlch3B0cLHTSEt4sgYVg5cwKgvP1CoUs" "u:my-ns.trusted-module.guard-func:mHbwuKeJZEAXZZhKUZolPtSNL-PAXNYBGkzRUUkeKsk")
    > 1.0
