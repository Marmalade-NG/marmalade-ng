.. _POLICY-NON-FUNGIBLE:

policy-non-fungible
-------------------

Description
^^^^^^^^^^^

This policy ensures that the token is non-fungible:
  - The precision is 0
  - The minted quantity is 1.0
  - It's impossible to "break" the token
  - There is always only at most a single owner of the token


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
Nope

View functions
^^^^^^^^^^^^^^
Nope

External request functions
^^^^^^^^^^^^^^^^^^^^^^^^^^
Nope
