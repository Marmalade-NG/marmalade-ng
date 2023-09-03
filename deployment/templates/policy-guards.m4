(namespace "__NAMESPACE__")

include(policies/policy-guards.pact)dnl
"Module loaded"

ifdef(`__INIT__',dnl
(create-table guards)
)
