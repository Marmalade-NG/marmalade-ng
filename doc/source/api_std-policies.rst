Module std-policies: Reference and API
=======================================

``std-policies`` is a module that contains **pure only** helpers to deal with
standard policies. The modules if intended to be used by wallets, market-places, tokens creator
to simplify development. It avoids manipulating directly modrefs, but instead allows manipulating simple strings.

Helpers
-------

to-policy
~~~~~~~~~
*name* ``string`` *→* ``module{token-policy-ng-v1``

Convert a policy name to the corresponding policy modref.

.. code:: lisp

  (use marmalade-ng.std-policies)
  (to-policy "INSTANT-MINT")
    > marmalade-ng.std-policies.instant-mint

list-to-policies
~~~~~~~~~~~~~~~~
*name* ``[string]`` *→* ``[module{token-policy-ng-v1}]``

Convert a list of policies names to a list of policies.

.. code:: lisp

  (use marmalade-ng.std-policies)
  (list-to-policies ["INSTANT-MINT", "ROYALTY"])
    > [marmalade-ng.std-policies.instant-mint marmalade-ng.std-policies.roaylty]

to-policies
~~~~~~~~~~~
*names* ``string`` *→* ``[module{token-policy-ng-v1}]``

Convert a list of names concatenated in a single string to a list of policies.

.. code:: lisp

  (use marmalade-ng.std-policies)
  (to-policies "INSTANT-MINT ROYALTY")
    > [marmalade-ng.std-policies.instant-mint, marmalade-ng.std-policies.roaylty]


from-policy
~~~~~~~~~~~~~
*policy* ``module{token-policy-ng-v1}`` *→* ``string``

Convert a policy name to the corresponding policy modref.

In case of a non standard policy, the string `UNKNOWN_xxxxxx` is returned.
where xxxxxx is an unique 6 hexadecimals characters derived from the hash of the policy


.. code:: lisp

  (use marmalade-ng.std-policies)
  (from-policy marmalade-ng.std-policies.instant-mint)
    > "INSTANT MINT"

  (from-policy private-ns.my-own-policy)
    > "UNKNOWN_AB147D"


.. _STD-POLICIES-POLICIES-TO-LIST:

policies-to-list
~~~~~~~~~~~~~~~~
 *policies* ``[module{token-policy-ng-v1}]`` *→* ``[string]``

Convert a list of policies names to a list of policies.

.. code:: lisp

  (use marmalade-ng.std-policies)
  (policies-to-list [marmalade-ng.std-policies.instant-mint, marmalade-ng.std-policies.roaylty])
    > ["INSTANT-MINT", "ROYALTY"]

.. _STD-POLICIES-FROM-POLICIES:

from-policies
~~~~~~~~~~~~~
 *policies* ``[module{token-policy-ng-v1}]`` *→*  ``string``

Convert a list of policies to a concatenated string.

.. code:: lisp

  (use marmalade-ng.std-policies)
  (from-policies [marmalade-ng.std-policies.instant-mint, private-ns.my-own-policy, marmalade-ng.std-policies.roaylty ])
    > "INSTANT-MINT UNKNOWN_AB147D ROYALTY"
