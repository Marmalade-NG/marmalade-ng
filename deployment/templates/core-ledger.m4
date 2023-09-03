define(`kip', __KIP_NAMESPACE__)dnl
(namespace "__NAMESPACE__")

include(ledger.pact)dnl
"Module loaded"

ifdef(`__INIT__',dnl
(create-table ledger)
(create-table tokens)
)
