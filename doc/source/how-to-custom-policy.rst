Create your own custom policy
-----------------------------

**Please read carefully, I repeat please read carefully.**

**Not following these recommendations could compromise the security of the tokens.**

More than 15 standard policies are available and should be preferably be used.

But to create specific behavior for a token, a creator may want to create his own
policy. And maybe combine them with standards policy.

However, the creator should be aware that marketplaces and wallets could have some difficulties
to handle tokens with custom policies.

Here is a policy template:

.. literalinclude:: ../../examples/cutom-policies/policy-template.pact
   :language: lisp


1 - Interface
~~~~~~~~~~~~~
The policy must implement ``token-policy-ng-v1`` and must include all the mandatory functions.
Importing ``token-info`` is necessary

2 - How to choose the rank
~~~~~~~~~~~~~~~~~~~~~~~~~~
If the policy only does enforcement, and does not manage sales or fees, ``0`` is the good value.

If the policy is involved in a sale, another value between 10 and 30 has to be chosen depending on how the
policy must be prioritized within the sale process vs *policy-marketplace*, *policy-royalty*, *policy-xxx-sale*, ...

3 - Ledger capabilities requirements
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In the template, each function starts with ``(require-capability (ledger.``.
This is the core of the Marmalade-NG subsystem.

Theses statements can safely be removed **ONLY** when the functions only include pure statements.
By the way, calling any modref is considered as impure. If unsure, you can without problems (but small gas consumption) keep
the ``(require-capability)`` statements.


4 - No cross policy-calls or dependencies
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Some would be tempted to share data and functions calls between policies (especially when some statements ``(require-capability)`` have been removed).

This is a bad practice and should definitely be avoided, as it could end to a vulnerability.
Instead, merging policies should be considered.

5 - Special hook enforce-sale-offer
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
If not used (ie the policy does not manage a sale like ``policy-xxxx-sale``), this function **MUST RETURN** ``false``.

6 - Reading data from the transaction
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
If the policy need to read data from the transaction, it should follow Marmalade-NG conventions.

In this case, the policy should use the functions ``(enforce-get-msg-data )`` and ``(get-msg-data )`` from the ``util-policies`` module.
This modules contains other function very useful to deal with *standard sale messages*.