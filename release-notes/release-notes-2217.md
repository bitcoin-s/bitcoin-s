### Configuration option changes required

It is required that all wallets pre 2217 need to have the configuration 

>bitcoin-s.wallet.aesPassword="changeMe"

See https://github.com/bitcoin-s/bitcoin-s/issues/2245

for the errors that are thrown when this isn't set.
