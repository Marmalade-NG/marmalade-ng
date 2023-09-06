.. _POLICY-FIXED-ISSUANCE:

policy-fixed-issuance
----------------------

Description
^^^^^^^^^^^

This policy ensures that a token follow an issuance spec:
- Precision
- Minimum mint amount
- Total maximum supply

It could make sense to associate with :ref:'POLICY-DISABLE-BURN'


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
