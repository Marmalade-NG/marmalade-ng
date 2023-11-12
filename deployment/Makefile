NAMESPACE = "n_442d3e11cfe0d39859878e5b1520cd8b8c36e5db"
KIP_NAMESPACE = "n_442d3e11cfe0d39859878e5b1520cd8b8c36e5db"

DATA_FILE = testnet_data.yaml

KEY_FILES = keys/a35.yaml \
            keys/e2e.yaml \
            keys/e97.yaml

INIT=-D__INIT__

TRANSACTION_FILES = interface-poly-fungible \
                    interface-token-policy \
                    core-ledger \
                    core-util-policies \
                    policy-adjustable-royalty \
                    policy-auction-sale \
                    policy-blacklist \
                    policy-collection \
                    policy-disable-burn \
                    policy-disable-sale \
                    policy-disable-transfer \
                    policy-dutch-auction-sale \
                    policy-extra-policies \
                    policy-fixed-issuance \
                    policy-fixed-sale \
                    policy-guards \
                    policy-instant-mint \
                    policy-marketplace \
                    policy-non-fungible \
                    policy-royalty \
                    policy-trusted-custody \
                    std-policies



.PRECIOUS: %.yaml .pact/%.pact


all: $(addsuffix .json, ${TRANSACTION_FILES})

.pact:
	mkdir -p .pact

%.json: %.yaml
	$(foreach kf,$(KEY_FILES),kda sign $< -k ${kf};)

.pact/%.pact: templates/%.m4 | .pact
	m4 -I../pact -D__NAMESPACE__=${NAMESPACE} -D__KIP_NAMESPACE__=${KIP_NAMESPACE} ${INIT} $< > $@

%.yaml: templates/%.tkpl .pact/%.pact
	kda gen -t $< -o $*.yaml -d ${DATA_FILE}

clean:
		rm -rf .pact
		rm -f *.json
		find . -maxdepth 1 -name "*.yaml" |grep -v "_data.yaml" | xargs rm -f
