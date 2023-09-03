(define-namespace "__NAMESPACE__" (read-keyset 'ks-ns) (read-keyset 'ks-ns))

(namespace "user")

(define-keyset "user.marm-ng-admin" (read-keyset 'ks-admin))

include(templates/interface-poly-fungible.m4)dnl
include(templates/interface-token-policy.m4)dnl
include(templates/core-ledger.m4)dnl
include(templates/core-util-policies.m4)dnl
include(templates/policy-adjustable-royalty.m4)dnl
include(templates/policy-auction-sale.m4)dnl
include(templates/policy-blacklist.m4)dnl
include(templates/policy-collection.m4)dnl
include(templates/policy-disable-burn.m4)dnl
include(templates/policy-disable-sale.m4)dnl
include(templates/policy-disable-transfer.m4)dnl
include(templates/policy-dutch-auction-sale.m4)dnl
include(templates/policy-fixed-issuance.m4)dnl
include(templates/policy-fixed-sale.m4)dnl
include(templates/policy-guards.m4)dnl
include(templates/policy-instant-mint.m4)dnl
include(templates/policy-marketplace.m4)dnl
include(templates/policy-non-fungible.m4)dnl
include(templates/policy-royalty.m4)dnl
include(templates/std-policies.m4)dnl
