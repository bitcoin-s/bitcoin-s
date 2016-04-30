This is the core functionality of bitcoin-s. 

This repostitory includes the following functionality:
  - Native Scala objects for various protocol types (Transaction, TransactionInput, ScriptSignatures...)
  - Serializers and deserializers for bitcoin data structures mentioned above
  - An implementation of Bitcoin's Script programming language 
    - This passes all of the test cases found inside of script_tests.json on the Bitcoin Core repo
    - Currently up to date through OP_CHECKLOCKTIMEVERIFY, OP_CHECKSEQUENCEVERIFY still needs to be implemented
  - 90% test coverage throughout the codebase to ensure high quality code. 
  - Functions documented with Scaladocs for user friendliness 
