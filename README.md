# testgen-hs

CBOR test case generator and deserializer for cross-checking other implementations.

## Example 1 – how to use it

```
❯ testgen-hs --help

Usage: testgen-hs COMMAND

  Test case generator for cross-checking CBOR (de)serializers

Available options:
  -h,--help                Show this help text

Available commands:
  generate                 Generate random CBOR test cases
  deserialize              Deserialize CBOR of ‘HardForkApplyTxErr’ that you got
                           from cardano-node
```

```
❯ testgen-hs generate --help

Usage: testgen-hs generate [-s|--seed SEED] [-g|--generator-size SIZE]
                           [-n|--number NUM] COMMAND

  Generate random CBOR test cases

Available options:
  -s,--seed SEED           Random seed integer (UNIX timestamp by default)
  -g,--generator-size SIZE Set the relative ‘size’ of the test cases
  -n,--number NUM          How many test cases to generate
  -h,--help                Show this help text

Available commands:
  ApplyTxErr_Byron         Generate CBOR of ApplyTxErr'Byron
  ApplyTxErr_Shelley       Generate CBOR of ApplyTxErr'Shelley
  ApplyTxErr_Allegra       Generate CBOR of ApplyTxErr'Allegra
  ApplyTxErr_Mary          Generate CBOR of ApplyTxErr'Mary
  ApplyTxErr_Alonzo        Generate CBOR of ApplyTxErr'Alonzo
  ApplyTxErr_Babbage       Generate CBOR of ApplyTxErr'Babbage
  ApplyTxErr_Conway        Generate CBOR of ApplyTxErr'Conway
  GHCInteger               Generate CBOR of GHCInteger
  DataText                 Generate CBOR of DataText
  ExampleADT               Generate CBOR of ExampleADT
```

```
❯ testgen-hs generate -g1 -n1 ApplyTxErr_Conway
```

```json
{
  "seed": 1731690796,
  "testCases": [
    {
      "cbor": "81820682820182018082038210581de0988807a48f6c7871bfa29d3ea70a6328c5fa35beb8c48589c676dc7b",
      "haskellRepr": "ApplyTxError (ConwayUtxowFailure (InvalidWitnessesUTXOW []) :| [ConwayGovFailure (ProposalReturnAccountDoesNotExist (RewardAccount {raNetwork = Testnet, raCredential = KeyHashObj (KeyHash {unKeyHash = \"988807a48f6c7871bfa29d3ea70a6328c5fa35beb8c48589c676dc7b\"})}))])",
      "json": {
        "contents": {
          "contents": {
            "contents": {
              "era": "ShelleyBasedEraConway",
              "error": [
                "ConwayUtxowFailure (InvalidWitnessesUTXOW [])",
                "ConwayGovFailure (ProposalReturnAccountDoesNotExist (RewardAccount {raNetwork = Testnet, raCredential = KeyHashObj (KeyHash {unKeyHash = \"988807a48f6c7871bfa29d3ea70a6328c5fa35beb8c48589c676dc7b\"})}))"
              ],
              "kind": "ShelleyTxValidationError"
            },
            "tag": "TxValidationErrorInCardanoMode"
          },
          "tag": "TxCmdTxSubmitValidationError"
        },
        "tag": "TxSubmitFail"
      },
      "typeTag": "ApplyTxError (ConwayEra StandardCrypto)"
    }
  ]
}
```

## Example 2 – searching for a particular error

With a little of Bash, it's possible to search for a particular error:

```bash
❯ while true ; do
    output=$(testgen-hs generate -g1 -n1 ApplyTxErr_Conway)
    if grep -q FeeTooSmall <<<"$output" ; then
      jq . <<<"$output"
      break
    fi
    sleep 0.01
  done
```

```json
{
  "seed": 1732539592414,
  "testCases": [
    {
      "cbor": "81820682830601008201820083051a000aa8a91a000291ff",
      "haskellRepr": "ApplyTxError (ConwayTxRefScriptsSizeTooBig 1 0 :| [ConwayUtxowFailure (UtxoFailure (FeeTooSmallUTxO (Coin 698537) (Coin 168447)))])",
      "json": {
        "contents": {
          "contents": {
            "contents": {
              "era": "ShelleyBasedEraConway",
              "error": [
                "ConwayTxRefScriptsSizeTooBig 1 0",
                "ConwayUtxowFailure (UtxoFailure (FeeTooSmallUTxO (Coin 698537) (Coin 168447)))"
              ],
              "kind": "ShelleyTxValidationError"
            },
            "tag": "TxValidationErrorInCardanoMode"
          },
          "tag": "TxCmdTxSubmitValidationError"
        },
        "tag": "TxSubmitFail"
      },
      "typeTag": "ApplyTxError (ConwayEra StandardCrypto)"
    }
  ]
}
```

And then easily reproduce it by providing the same seed that was found:

```
❯ testgen-hs generate -s1732539592414 -g1 -n1 ApplyTxErr_Conway
```

```json
{
  "seed": 1732539592414,
  "testCases": [
    {
      "cbor": "81820682830601008201820083051a000aa8a91a000291ff",
      "haskellRepr": "ApplyTxError (ConwayTxRefScriptsSizeTooBig 1 0 :| [ConwayUtxowFailure (UtxoFailure (FeeTooSmallUTxO (Coin 698537) (Coin 168447)))])",
      "json": {
        "contents": {
          "contents": {
            "contents": {
              "era": "ShelleyBasedEraConway",
              "error": [
                "ConwayTxRefScriptsSizeTooBig 1 0",
                "ConwayUtxowFailure (UtxoFailure (FeeTooSmallUTxO (Coin 698537) (Coin 168447)))"
              ],
              "kind": "ShelleyTxValidationError"
            },
            "tag": "TxValidationErrorInCardanoMode"
          },
          "tag": "TxCmdTxSubmitValidationError"
        },
        "tag": "TxSubmitFail"
      },
      "typeTag": "ApplyTxError (ConwayEra StandardCrypto)"
    }
  ]
}
```

## Example 3 – two small Conway errors

```
❯ testgen-hs generate -s1732187056519 -g1 -n1 ApplyTxErr_Conway
```

```json
{
  "seed": 1732187056519,
  "testCases": [
    {
      "cbor": "818206828201820083051a000151351a00074b8582076162",
      "haskellRepr": "ApplyTxError (ConwayUtxowFailure (UtxoFailure (FeeTooSmallUTxO (Coin 86325) (Coin 478085))) :| [ConwayMempoolFailure \"b\"])",
      "json": {
        "contents": {
          "contents": {
            "contents": {
              "era": "ShelleyBasedEraConway",
              "error": [
                "ConwayUtxowFailure (UtxoFailure (FeeTooSmallUTxO (Coin 86325) (Coin 478085)))",
                "ConwayMempoolFailure \"b\""
              ],
              "kind": "ShelleyTxValidationError"
            },
            "tag": "TxValidationErrorInCardanoMode"
          },
          "tag": "TxCmdTxSubmitValidationError"
        },
        "tag": "TxSubmitFail"
      },
      "typeTag": "ApplyTxError (ConwayEra StandardCrypto)"
    }
  ]
}
```

```
❯ testgen-hs generate -s1732186210861 -g1 -n1 ApplyTxErr_Conway
```

```json
{
  "seed": 1732186210861,
  "testCases": [
    {
      "cbor": "818206828201820083051a000a63d11a0009580683051a0008d00a1a0001ebc4",
      "haskellRepr": "ApplyTxError (ConwayUtxowFailure (UtxoFailure (FeeTooSmallUTxO (Coin 680913) (Coin 612358))) :| [ConwayTreasuryValueMismatch (Coin 577546) (Coin 125892)])",
      "json": {
        "contents": {
          "contents": {
            "contents": {
              "era": "ShelleyBasedEraConway",
              "error": [
                "ConwayUtxowFailure (UtxoFailure (FeeTooSmallUTxO (Coin 680913) (Coin 612358)))",
                "ConwayTreasuryValueMismatch (Coin 577546) (Coin 125892)"
              ],
              "kind": "ShelleyTxValidationError"
            },
            "tag": "TxValidationErrorInCardanoMode"
          },
          "tag": "TxCmdTxSubmitValidationError"
        },
        "tag": "TxSubmitFail"
      },
      "typeTag": "ApplyTxError (ConwayEra StandardCrypto)"
    }
  ]
}
```

## Example 4 – deserialize a CBOR

```
❯ testgen-hs deserialize 818206828201820083051a000a63d11a0009580683051a0008d00a1a0001ebc4
```

```json
{
  "cbor": "818206828201820083051a000a63d11a0009580683051a0008d00a1a0001ebc4",
  "haskellRepr": "ApplyTxError (ConwayUtxowFailure (UtxoFailure (FeeTooSmallUTxO (Coin 680913) (Coin 612358))) :| [ConwayTreasuryValueMismatch (Coin 577546) (Coin 125892)])",
  "json": {
    "contents": {
      "contents": {
        "contents": {
          "era": "ShelleyBasedEraConway",
          "error": [
            "ConwayUtxowFailure (UtxoFailure (FeeTooSmallUTxO (Coin 680913) (Coin 612358)))",
            "ConwayTreasuryValueMismatch (Coin 577546) (Coin 125892)"
          ],
          "kind": "ShelleyTxValidationError"
        },
        "tag": "TxValidationErrorInCardanoMode"
      },
      "tag": "TxCmdTxSubmitValidationError"
    },
    "tag": "TxSubmitFail"
  },
  "typeTag": "ApplyTxError (ConwayEra StandardCrypto)"
}
```

## Example 5 – deserialize a stream of CBORs

```
❯ ( echo 8182068182028200a0
    echo 81820681820481581cc0231342a5c66b25d652a7116559d02cbe9515ef890fd698de38d456
  ) | testgen-hs deserialize-stream
```

```json
{"cbor":"8182068182028200a0","haskellRepr":"ApplyTxError (ConwayCertsFailure (WithdrawalsNotInRewardsCERTS (fromList [])) :| [])","json":{"contents":{"contents":{"contents":{"era":"ShelleyBasedEraConway","error":["ConwayCertsFailure (WithdrawalsNotInRewardsCERTS (fromList []))"],"kind":"ShelleyTxValidationError"},"tag":"TxValidationErrorInCardanoMode"},"tag":"TxCmdTxSubmitValidationError"},"tag":"TxSubmitFail"},"typeTag":"ApplyTxError (ConwayEra StandardCrypto)"}
{"cbor":"81820681820481581cc0231342a5c66b25d652a7116559d02cbe9515ef890fd698de38d456","haskellRepr":"ApplyTxError (ConwayWdrlNotDelegatedToDRep (KeyHash {unKeyHash = \"c0231342a5c66b25d652a7116559d02cbe9515ef890fd698de38d456\"} :| []) :| [])","json":{"contents":{"contents":{"contents":{"era":"ShelleyBasedEraConway","error":["ConwayWdrlNotDelegatedToDRep (KeyHash {unKeyHash = \"c0231342a5c66b25d652a7116559d02cbe9515ef890fd698de38d456\"} :| [])"],"kind":"ShelleyTxValidationError"},"tag":"TxValidationErrorInCardanoMode"},"tag":"TxCmdTxSubmitValidationError"},"tag":"TxSubmitFail"},"typeTag":"ApplyTxError (ConwayEra StandardCrypto)"}
```
