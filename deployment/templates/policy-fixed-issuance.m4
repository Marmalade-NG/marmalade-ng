(namespace "__NAMESPACE__")

include(policies/policy-fixed-issuance.pact)dnl
"Module loaded"

ifdef(`__INIT__',dnl
(create-table supplies)
)
