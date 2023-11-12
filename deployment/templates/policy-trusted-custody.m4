(namespace "__NAMESPACE__")

include(policies/policy-trusted-custody.pact)dnl
"Module loaded"

ifdef(`__INIT__',dnl
(create-table tokens-custody)
(create-table accounts-custody)
)
