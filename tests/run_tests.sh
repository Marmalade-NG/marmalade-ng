#!/bin/bash
PACT="${PACT_BIN:-pact}"

case $1 in
"--short")
  POSTPROCESS="tail -1";;
*)
  POSTPROCESS="cat";;
esac

REPL_SCRIPTS="./test-ledger.repl
              ./test-util-policies.repl
              ./test-std-policies.repl
              ./test-policy-adjustable-royalty.repl
              ./test-policy-auction-sale.repl
              ./test-policy-blacklist.repl
              ./test-policy-collection.repl
              ./test-policy-disable-burn.repl
              ./test-policy-disable-transfer.repl
              ./test-policy-dutch-auction-sale.repl
              ./test-policy-instant-mint.repl
              ./test-policy-fixed-issuance.repl
              ./test-policy-fixed-sale.repl
              ./test-policy-marketplace.repl
              ./test-policy-non-fungible.repl
              ./test-policy-guards.repl
              ./test-policy-royalty.repl "

for repl in $REPL_SCRIPTS
  do echo "============================================================"
     echo "Running $repl"
     echo "============================================================"
     ${PACT} $repl 2>&1 | $POSTPROCESS
     echo ""
done
