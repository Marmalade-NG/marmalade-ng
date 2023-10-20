.. _POLICY-EXTRA-POLICIES:

policy-extra-policies
---------------------

Description
^^^^^^^^^^^

This policy acts as a proxy for calling the extra-policies.

The administrator can add policies to the global list.

The token issuer can remove them by blacklisting them on a per token basis.

The blacklist is protected by guard set at token creation. This guard can
be used by child policies.


Implemented hooks
^^^^^^^^^^^^^^^^^

.. code:: lisp

  (defun enforce-init)

  (defun enforce-mint)

  (defun enforce-burn)

  (defun enforce-transfer)

  (defun enforce-sale-offer)

  (defun enforce-sale-buy)

  (defun enforce-sale-settle)


Input data structures
^^^^^^^^^^^^^^^^^^^^^

marmalade_extra_polices
~~~~~~~~~~~~~~~~~~~~~~~
Handled by ``(enforce-sale-init)``

.. code:: lisp

  (defschema extra-policies-msg-sch
    guard:guard ;Owner by the creator: used to protect blacklists
  )


This object is mandatory during creation.

External functions
^^^^^^^^^^^^^^^^^^

.. _POLICY-EXTRA-POLICIES-GET-GUARD:

get-guard
~~~~~~~~~~
*token* ``object{token-info}`` *→* ``guard``

Return the guard assiocated to a token

get-guard-by-id
~~~~~~~~~~~~~~~
*token-id* ``string`` *→* ``guard``

Return the guard assiocated to a token-id

add-to-blacklist
~~~~~~~~~~~~~~~~
*token-id* ``string`` *policy* ``module{token-policy-ng-v1}`` *→* ``string``

Add a new policy to the blacklist for the given token-id.

Must be signed by the creator's guard. The signature can be scopped
by ``(UPDATE-EXTRA-POLICIES token-id)``.

.. code:: lisp

  (use marmalade-ng.policy-extra-policies)
  (add-to-blacklist "t:fFyEf-Tu_b_DKe9RJDbwvMPX2-sFe2lMNuDcH4UGJ10" marmalade-ng-extra.my-policy)

remove-from-blacklist
~~~~~~~~~~~~~~~~~~~~~
*token-id* ``string`` *policy* ``module{token-policy-ng-v1}`` *→* ``string``

Remove of policy from the blacklist for the given token-id.

Must be signed by the creator's guard. The signature can be scopped
by ``(UPDATE-EXTRA-POLICIES token-id)``.

.. code:: lisp

  (use marmalade-ng.policy-extra-policies)
  (remove-blacklist "t:fFyEf-Tu_b_DKe9RJDbwvMPX2-sFe2lMNuDcH4UGJ10" marmalade-ng-extra.my-policy)


rotate
~~~~~~
*token-id* ``string`` *new-guard* ``guard`` *→* ``string``

Rotate the extra policy guard for the given token-id.

Must be signed by the creator's guard. The signature can be scopped
by ``(UPDATE-EXTRA-POLICIES token-id)``.

.. code:: lisp

  (use marmalade-ng.policy-extra-policies)
  (rotate "t:fFyEf-Tu_b_DKe9RJDbwvMPX2-sFe2lMNuDcH4UGJ10" (keyset-ref-guard "user.my-creator-name"))


View functions
^^^^^^^^^^^^^^
list-registered-policies
~~~~~~~~~~~~~~~~~~~~~~~~
*→* ``[module{token-policy-ng-v1}]``

List all global policies.

.. code:: lisp

  (use marmalade-ng.policy-extra-policies)
  (list-registered-policies)
    > [marmalade-ng-extra.my-policy, marmalade-ng-extra.new-sale-policy]

get-blacklist
~~~~~~~~~~~~~
*token-id* ``string`` *→* ``[module{token-policy-ng-v1}]``

List all global policies.

.. code:: lisp

  (use marmalade-ng.policy-extra-policies)
  (get-blacklist "t:fFyEf-Tu_b_DKe9RJDbwvMPX2-sFe2lMNuDcH4UGJ10")
    > [marmalade-ng-extra.my-policy]




Administrative functions
^^^^^^^^^^^^^^^^^^^^^^^^
register-policy
~~~~~~~~~~~~~~~
*policy* ``module{token-policy-ng-v1}`` *→* ``string``

Admin function to add a policy to the global extra-policies list.

The transaction must be signed by the extra-policy governance keyset.
The signature can be scopped by ``(EXTRA-POLICIES-GOVERNANCE)``.

.. code:: lisp

  (use marmalade-ng.policy-extra-policies)
  (register-policy marmalade-ng-extra.my-policy)


unregister-policy
~~~~~~~~~~~~~~~~~
*policy* ``module{token-policy-ng-v1}`` *→* ``string``

Admin function to remove a policy from the global extra-policies list.
The signature can be scopped by ``(EXTRA-POLICIES-GOVERNANCE)``.

.. code:: lisp

  (use marmalade-ng.policy-extra-policies)
  (unregister-policy marmalade-ng-extra.my-policy)
