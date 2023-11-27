.. _POLICY-COLLECTION:

policy-collection
-----------------

Description
^^^^^^^^^^^

This policy allows to creating collections of tokens.

A collection is defined by an unique collection ID.

The collection-id has the following form: ``c_${name}_${hash}``

where:
  - ${name} is the human-readable name of the collection.
  - ${hash} is a hash derived from the collection guard and name.

The collection ID is protected from front-running and unauthorized use or declaration.
However, the collection-human readable name has no protection and can be reused by anybody else.
As such, wallets, market-places should only rely on the collection ID.


A collection is protected by a guard which prohibits someone other than the collection
creator to include tokens. The guard is enforced on token creation.

If the guard is a keyset, the signature may be scoped to the capability:
``(ADD-TO-COLLECTION collection-id token-id)``

When created a collection must be registered with its creator's name.
If the creator's name is a principal (recommended), it is validated against the guard, and can
be trusted by third parties.


Ranks
~~~~~
Each token when created inside a collection is given automatically a rank.

Ranks start with #1 and are immutable.


Collection sizes and maximum tokens
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Each collection has a defined size limit. This size is defined during the collection
creation and is then immutable.

This protects owners and potential buyers. They can be sure that the value of their token
won't be diluted in a larger collection than they expect.

The special value: ``UNLIMITED-SIZE`` (currently defined to ``0``) can be used to
create unlimited collection.


Implemented hooks
^^^^^^^^^^^^^^^^^

.. code:: lisp

  (defun enforce-init)

Input data structures
^^^^^^^^^^^^^^^^^^^^^
collection
~~~~~~~~~~
Handled by ``(enforce-init)``

.. code:: lisp

  (defschema collection-msg-sch
    id:string
  )

Mandatory



External functions
^^^^^^^^^^^^^^^^^^
create-collection-id
~~~~~~~~~~~~~~~~~~~~
*name* ``string`` *creator-guard* ``guard`` *→* ``string``

Pure function

Create a collection-id from an human-readable name.

.. code:: lisp

  (use marmalade-ng.policy-collection)
  (create-collection-id "PrettyKitties" (keyset-ref-guard "user.pretty-kitties-owner"))
    > "c_PrettyKitties_e8XfSKUAM1fZ8HkaM1FqaYIc6v-xPUF1S2qyzz6vqQs"

create-collection
~~~~~~~~~~~~~~~~~
*id* ``string`` *name* ``string`` *size* ``integer`` *creator* ``string`` *creator-guard* ``guard`` *→* ``bool``

Create and register a collection.

The creator guard will be enforced.

*size* is the maximum number of tokens that can be contained in the collection.
If *size* is ``UNLIMITED-SIZE`` (currently defined to ``0``), the collection is unlimited.

.. code:: lisp

  (use marmalade-ng.policy-collection)
  (create-collection "c_PrettyKitties_e8XfSKUAM1fZ8HkaM1FqaYIc6v-xPUF1S2qyzz6vqQs"
                      "PrettyKitties" 112 "r:user.pretty-kitties-owner"
                      (keyset-ref-guard "user.pretty-kitties-owner"))

View functions
^^^^^^^^^^^^^^
get-collection
~~~~~~~~~~~~~~
*collection-id* ``string`` *→* ``object{collection-sch}``

Get collection details from a collection-id.

.. code:: lisp

  (use marmalade-ng.policy-collection)
  (get-collection "c_Cats_ZMLLJuSq0JoHSR4f_ZgUa2H_p7Rr71CN8CjQ7ZL_hU0")

.. code::

  {"creator": "k:1caa4f5f12ea490f8f020734ed08be1926f290855818e19abfaf6dc8d03ce798",
   "creator-guard": KeySet {keys: ["1caa4f5f12ea490f8f020734ed08be1926f290855818e19abfaf6dc8d03ce798"],
                            pred: keys-all},
   "id": "c_Cats_ZMLLJuSq0JoHSR4f_ZgUa2H_p7Rr71CN8CjQ7ZL_hU0",
   "max-size": 0,
   "name": "Cats",
   "size": 3
  }

.. _POLICY-COLLECTION-GET-TOKEN-COLLECTION:

get-token-collection
~~~~~~~~~~~~~~~~~~~~
*token-id* ``string`` *→* ``object{collection-sch}``

Get collection details of a token.

.. code:: lisp

  (use marmalade-ng.policy-collection)
  (get-token-collection "t:MkygmZK2iaGHuTTmKnzJMke3HcALz8SgTyxnD5A-VkA")

.. code::

  {"creator": "k:1caa4f5f12ea490f8f020734ed08be1926f290855818e19abfaf6dc8d03ce798",
   "creator-guard": KeySet {keys: ["1caa4f5f12ea490f8f020734ed08be1926f290855818e19abfaf6dc8d03ce798"],
                            pred: keys-all},
   "id": "c_Cats_ZMLLJuSq0JoHSR4f_ZgUa2H_p7Rr71CN8CjQ7ZL_hU0",
   "max-size": 0,
   "name": "Cats",
   "size": 3
  }

get-all-collections
~~~~~~~~~~~~~~~~~~~~
  *→* ``[string]``

Return all collection-ids of the system.

**Important**: Local only function. Do not use in transactions.

.. code:: lisp

  (use marmalade-ng.policy-collection)
  (get-all-collections)
    > ["c_Dogs_8BRJPRYtqM-2w8ASMYq6Toq4PIvhws-kHh0nbYfT1iY", "c_Cats_ZMLLJuSq0JoHSR4f_ZgUa2H_p7Rr71CN8CjQ7ZL_hU0"]


get-collections-by-creator
~~~~~~~~~~~~~~~~~~~~~~~~~~
*creator* ``string`` *→* ``object{collection-sch}``

Return the list of all collection objects owned by a creator.

.. code:: lisp

  (use marmalade-ng.policy-collection)
  (get-collections-by-creator "k:1caa4f5f12ea490f8f020734ed08be1926f290855818e19abfaf6dc8d03ce798")

.. code::

  [ {"creator": "k:1caa4f5f12ea490f8f020734ed08be1926f290855818e19abfaf6dc8d03ce798",
     "creator-guard": KeySet {keys: ["1caa4f5f12ea490f8f020734ed08be1926f290855818e19abfaf6dc8d03ce798"],
                              pred: keys-all},
     "id": "c_Cats_ZMLLJuSq0JoHSR4f_ZgUa2H_p7Rr71CN8CjQ7ZL_hU0",
     "max-size": 0,
     "name": "Cats",
     "size": 3
    },
    {"creator": "k:1caa4f5f12ea490f8f020734ed08be1926f290855818e19abfaf6dc8d03ce798",
     "creator-guard": KeySet {keys: ["1caa4f5f12ea490f8f020734ed08be1926f290855818e19abfaf6dc8d03ce798"],
                              pred: keys-all},
     "id": "c_WildCats_G_X53tGkoawB8WDvJdTvlMG_VWmHeYZVieS-n5DUi9U",
     "max-size": 0,
     "name": "WildCats",
     "size": 3
    }]


get-token-rank-in-collection
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
*token-id* ``string`` *→* ``int``

Return the rank of a given token inside the collection.

.. code:: lisp

  (use marmalade-ng.policy-collection)
  (get-token-rank-in-collection "t:MkygmZK2iaGHuTTmKnzJMke3HcALz8SgTyxnD5A-VkA")
    > 2


list-tokens-of-collection
~~~~~~~~~~~~~~~~~~~~~~~~~
*collection-id* ``string`` *→* ``[string]``

List all token-ids belonging to a collection.

Tokens are sorted by rank.

**Important**: Local only function. Do not use in transactions.

.. code:: lisp

  (use marmalade-ng.policy-collection)
  (list-tokens-of-collection "c_Dogs_8BRJPRYtqM-2w8ASMYq6Toq4PIvhws-kHh0nbYfT1iY")
    > ["t:C6KzoW9DSYdSbrZuUOazHzlpdA_vtZyiwFo9WTIUhP8",
       "t:MkygmZK2iaGHuTTmKnzJMke3HcALz8SgTyxnD5A-VkA",
       "t:Axc6q-aWpN9g1u3NveIXaNMOqJ5n_Wsudw4GDnFiZNM"]


list-tokens-of-collections
~~~~~~~~~~~~~~~~~~~~~~~~~~
*collection-ids* ``[string]`` *→* ``[string]``

List all token-ids belonging to a list of collections.

The order of the tokens in the result list is undefined.


**Important**: Local only function. Do not use in transactions.

.. code:: lisp

  (use marmalade-ng.policy-collection)
  (list-tokens-of-collections ["c_Dogs_8BRJPRYtqM-2w8ASMYq6Toq4PIvhws-kHh0nbYfT1iY",
                               "c_Cats_ZMLLJuSq0JoHSR4f_ZgUa2H_p7Rr71CN8CjQ7ZL_hU0"])
    > ["t:C6KzoW9DSYdSbrZuUOazHzlpdA_vtZyiwFo9WTIUhP8",
       "t:MkygmZK2iaGHuTTmKnzJMke3HcALz8SgTyxnD5A-VkA",
       "t:Axc6q-aWpN9g1u3NveIXaNMOqJ5n_Wsudw4GDnFiZNM"]


Events
^^^^^^
CREATE-COLLECTION
~~~~~~~~~~~~~~~~~
*collection-id* ``string`` *collection-name* ``string`` *collection-size* ``integer`` *creator* ``string``

Emitted when a collection is created.


ADD-TO-COLLECTION
~~~~~~~~~~~~~~~~~
*collection-id* ``string`` *token-id* ``string``

Emitted when a token is added to a collection.
