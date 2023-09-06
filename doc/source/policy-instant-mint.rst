.. _POLICY-INSTANT-MINT:

policy-instant-mint
-------------------

Description
^^^^^^^^^^^

This policy ensures that a token is minted just after creation.

The ``(token-create)`` and ``(mint)`` must occur **in the same transaction**

Further minting is prohibited.

Implemented hooks
^^^^^^^^^^^^^^^^^

.. code:: lisp

  (defun enforce-init)

  (defun enforce-mint)



Input data structures
^^^^^^^^^^^^^^^^^^^^^
Nope

External functions
^^^^^^^^^^^^^^^^^^
Nope

View functions
^^^^^^^^^^^^^^
Nope
