(namespace "__NAMESPACE__")

include(policies/policy-instant-mint.pact)dnl
"Module loaded"

ifdef(`__INIT__',dnl
(create-table instant-mint)
)
