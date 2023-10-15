(namespace "__NAMESPACE__")

include(policies/policy-extra-policies.pact)dnl
"Module loaded"

ifdef(`__INIT__',dnl
(create-table global)
(create-table tokens)
(create-table sales)
(init)
)
