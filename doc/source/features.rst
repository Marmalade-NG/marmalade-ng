List of standard features/policies
==================================

Marmalade-NG includes a comprehensive set of policies, which should cover most basic
use cases. Removing the requirement for most token creator to create a custom a policy.

.. list-table:: General policies
   :widths: 25 25 50
   :header-rows: 1

   * - Policy
     - Name (std-policies)
     - Description

   * - :ref:`POLICY-COLLECTION`
     - COLLECTION
     - Allow the issuer to create collections of tokens

   * - :ref:`POLICY-BLACKLIST`
     - BLACKLIST
     - Allow to create blacklistable tokens

   * - :ref:`POLICY-GUARDS`
     - GUARDS
     - Allow to protect a token by a set of guards



.. list-table:: Minting restrictions policies
   :widths: 25 25 50
   :header-rows: 1

   * - Policy
     - Name (std-policies)
     - Description

   * - :ref:`POLICY-INSTANT-MINT`
     - INSTANT-MINT
     - Only allow tokens to be minted in the same transaction as they are created

   * - :ref:`POLICY-NON-FUNGIBLE`
     - NON-FUNGIBLE
     - Only allow to mint non-fungible tokens

   * - :ref:`POLICY-FIXED-ISSUANCE`
     - FIXED-ISSUANCE
     - Allow to define and enforce an issuance specification


.. list-table:: Other restrictions policies
   :widths: 25 25 50
   :header-rows: 1

   * - Policy
     - Name (std-policies)
     - Description

   * - :ref:`POLICY-DISABLE-BURN`
     - DISABLE-BURN
     - Disable burn

   * - :ref:`POLICY-DISABLE-TRANSFER`
     - DISABLE-TRANSFER
     - Disable transfer

.. list-table:: Sales policies
   :widths: 25 25 50
   :header-rows: 1

   * - Policy
     - Name (std-policies)
     - Description

   * - :ref:`POLICY-DISABLE-SALE`
     - DISABLE-SALE
     - Disable any sale

   * - :ref:`POLICY-FIXED-SALE`
     - FIXED-SALE
     - Manage fixed quote sales

   * - :ref:`POLICY-DUTCH-AUCTION-SALE`
     - DUTCH-AUCTION-SALE
     - Manage dutch auction sales (automatic price reduction)

   * - :ref:`POLICY-AUCTION-SALE`
     - AUCTION-SALE
     - Manage auction sales

.. list-table:: Fees policies
   :widths: 25 25 50
   :header-rows: 1

   * - Policy
     - Name (std-policies)
     - Description

   * - :ref:`POLICY-ROYALTY`
     - ROYALTY
     - Allow the creator to impose a fixed royalty

   * - :ref:`POLICY-ADJUSTABLE-ROYALTY`
     - ADJUSTABLE-ROYALTY
     - Allow the creator to impose a royalty which can be changed during the life of the token

   * - :ref:`POLICY-MARKETPLACE`
     - MARKETPLACE
     - Allow a market-place to fix a fee on a sale
