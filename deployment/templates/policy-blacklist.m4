(namespace "__NAMESPACE__")

include(policies/policy-blacklist.pact)dnl
"Module loaded"

ifdef(`__INIT__',dnl
(create-table blacklist)
(create-table tokens-blacklist)
)
