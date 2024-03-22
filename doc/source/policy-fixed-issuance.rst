.. _POLICY-FIXED-ISSUANCE:

policy-fixed-issuance
----------------------

Description
^^^^^^^^^^^

This policy ensures that a token follow an issuance spec:
  - Precision
  - Minimum mint amount
  - Total maximum supply

It could make sense to associate with :ref:`POLICY-DISABLE-BURN`. This policy is
not compatible with :ref:`POLICY-NON-FUNGIBLE`

This policy allows easy creation of poly-fungible-tokens.

Examples:
^^^^^^^^^
A token with a total supply of 1.0, which can be divided into 100 parts:
  - Precision = 2
  - Minimum mint amount= 0.01
  - Max supply = 1.0

A token with a total supply of 41.0 which can be divided into 41 parts.
  - *Usually, a full token is represented by a supply of 1.0. But here with 41 shares, it would be impossible.
    That's why we switch to a precision of 0, and shares represented by a unity.*
  - Precision = 0
  - Minimum mint amount= 1.0
  - Max supply = 41.0

Implemented hooks
^^^^^^^^^^^^^^^^^

.. code:: lisp

  (defun enforce-init)

  (defun enforce-mint)


Input data structures
^^^^^^^^^^^^^^^^^^^^^
fixed_supply
~~~~~~~~~~~~

Handled by ``(create-token)``

.. code:: lisp

  (defschema fixed-issuance-msg-sch
   max_supply:decimal
   min_mint_amount:decimal
   precision:integer
  )


External functions
^^^^^^^^^^^^^^^^^^
Nope

View functions
^^^^^^^^^^^^^^
.. _POLICY-FIXED-ISSUANCE-GET-ISSUANCE-SPEC:

get-issuance-spec
~~~~~~~~~~~~~~~~~
*token-id* ``string`` *→* ``object{fixed-issuance-sch}``

Return the issuance specification of a given token.

.. code:: lisp

  (use marmalade-ng.policy-fixed-issuance)
  (get-issuance-spec "t:r-4jQUrZWpYfEqq_iGvBn1ofgCLPoh2ZfG5kfAVX2KM")
    > {"max-supply":1.0,
       "min-mint-amount":0.1}
