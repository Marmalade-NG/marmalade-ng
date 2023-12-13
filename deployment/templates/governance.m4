(namespace "__NAMESPACE__")

include(governance.pact)dnl
"Module loaded"

ifdef(`__INIT__',dnl
(define-namespace "__NAMESPACE__" (create-user-guard (governance.namespace-user-guard)) ns.GUARD_FAILURE)
)
(format "Namespace lock state:{}" [__NAMESPACE__.governance.NAMESPACE-LOCK])
