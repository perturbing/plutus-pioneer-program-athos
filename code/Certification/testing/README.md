# Setup
This directory is for testing purposes and not in the final product. The goal of the two files in this repository is to generate the artifacts needed by the `Start` and `Mint` tools.

In the `participants.json` we can add participants. The `generate-test-data.ts` file will convert this file and write `data/mint-merkleTree-data.ts` which is used by the `Mint` tool and `data/start-merkleTree-Data.ts` which is used by the `Start` tool.

Alternatively, we can also generate a set of `n` participants using the `generate-participants.ts` file.
