(namespace "__NAMESPACE__")

include(policies/policy-marketplace.pact)dnl
"Module loaded"

ifdef(`__INIT__',dnl
(create-table marketplace-sales)
)
