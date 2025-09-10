This DSL manages a simple library system with books, users, and categories. Operations include adding/removing books, listing inventory, and checkout/return. Categories can be nested like Fiction(Fantasy(Epic)) for hierarchical organization.

Examples:
add book Algorithms Sedgewick Technical(Programming(DataStructures))
remove book Hobbit
checkout Dune Alice
return Dune
