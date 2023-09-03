(namespace "__NAMESPACE__")

include(policies/policy-auction-sale.pact)dnl
"Module loaded"

ifdef(`__INIT__',dnl
(create-table auctions)
)
