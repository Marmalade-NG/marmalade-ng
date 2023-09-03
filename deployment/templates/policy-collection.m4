(namespace "__NAMESPACE__")

include(policies/policy-collection.pact)dnl
"Module loaded"

ifdef(`__INIT__',dnl
(create-table collections)
(create-table tokens)
)
