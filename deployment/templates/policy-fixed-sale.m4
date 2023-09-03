(namespace "__NAMESPACE__")

include(policies/policy-fixed-sale.pact)dnl
"Module loaded"

ifdef(`__INIT__',dnl
(create-table quotes)
)
