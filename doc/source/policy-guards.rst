.. _POLICY-GUARDS:

policy-guards
-------------

Description
^^^^^^^^^^^

This policy allows the creator to limit some functions by adding guards.

The guards can be any of the guards supported by Pact:
   - keysets
   - reference keysets
   - user guards


Thus, this policy can be very flexible. It can just help the creator to limit to himself some functions
like ``mint``.

But by using user guards, the creator can create more complex scenarios without creating a policy.
The policy guard can be used as a *gateway* between Marmalade NG and another module.

The policy allows to define 4 guards:
  - mint guard. In case of a signature it can be scoped by ``(marmalade-ng.policy-guards.MINT token_id )``
  - burn guard. In case of a signature it can be scoped by ``(marmalade-ng.policy-guards.BURN token_id )``
  - transfer guard. In case of a signature it can be scoped by ``(marmalade-ng.policy-guards.TRANSFER token_id )``
  - sale guard. In case of a signature it can be scoped by ``(marmalade-ng.policy-guards.SALE token_id )``


The policy module provide two helpers function:
  - An *always pass* guard (bypass): use ``{"fun":"marmalade-ng.policy-guards.success", "args":[]}``
  - An *always fail* guard (prohibit): use ``{"fun":"marmalade-ng.policy-guards.failure", "args":[]}``


**Note:** Since the guards are immutable, it is recommended to not use bare keysets.
But instead, it is preferable to use reference to named keysets to allow further rotation.


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
guards
~~~~~~

Handled by ``(create-token)``

.. code:: lisp

  (defschema guards-sch
    mint:guard
    burn:guard
    sale:guard
    transfer:guard
  )

At token creation, this object defines the guards that will be checked during subsequent operations.


Example: Restrict minting and burning to the token owner, allow sale and prohibit transfer

.. code:: json

  {"marmalade_guards": {"mint": {"keysetref":{"ns":"user","ksn":"token-owner"}},
                        "burn": {"keysetref":{"ns":"user","ksn":"token-owner"}},
                        "sale": {"fun":"marmalade-ng.policy-guards.success", "args":[]},
                        "transfer":{"fun":"marmalade-ng.policy-guards.failure", "args":[]}
                        }
   }


External functions
^^^^^^^^^^^^^^^^^^
Nope

View functions
^^^^^^^^^^^^^^
get-guards
~~~~~~~~~~
*token-id* ``string`` *→* ``object{guards-sch}``

Return the guard object for a given token-id
