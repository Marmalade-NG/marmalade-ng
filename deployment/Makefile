#testnet

#NAMESPACE = "n_442d3e11cfe0d39859878e5b1520cd8b8c36e5db"
#KIP_NAMESPACE = "n_5caec7ffe20c6e09fd632fb1ee04468848966332"
#DATA_FILE = testnet_data.yaml

KEY_FILES = keys/a35.yaml \
            keys/e2e.yaml \
            keys/e97.yaml

#mainnet
NAMESPACE = "n_4e470a97222514a8662dd1219000a0431451b0ee"
KIP_NAMESPACE = "n_4e47017ea6449649e13d79906dd1697fb1fe1d7a"
DATA_FILE = mainnet_data.yaml

KEY_FILES =

INIT=-D__INIT__

TRANSACTION_FILES = interface-poly-fungible \
                    interface-token-policy \
										governance \
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



.PRECIOUS: tx_%.yaml .pact/%.pact


all: $(addprefix tx_, $(addsuffix .json, ${TRANSACTION_FILES}))
yaml: $(addprefix tx_, $(addsuffix .yaml, ${TRANSACTION_FILES}))

.pact:
	mkdir -p .pact

%.json: %.yaml
	$(foreach kf,$(KEY_FILES),kda sign $< -k ${kf};)

.pact/%.pact: templates/%.m4 | .pact
	m4 -I../pact -D__NAMESPACE__=${NAMESPACE} -D__KIP_NAMESPACE__=${KIP_NAMESPACE} ${INIT} $< > $@

tx_%.yaml: templates/%.tkpl .pact/%.pact
	kda gen -t $< -o $@ -d ${DATA_FILE}

clean:
		rm -rf .pact
		rm -f *.json
		find . -maxdepth 1 -name "*.yaml" |grep -v "_data.yaml" | xargs rm -f
