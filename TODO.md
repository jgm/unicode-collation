# Profiling reveals:

6.8%  rootCollation
8.1%  toSortKey
13.7% matchLongestPrefix.go
5.1%  getCollationElements.go
4.0%  canonicalCombiningClass
23.2% Data.Text.Normalize.normalize
10%   doVariable

# Weird test failures with tibetan

 toHex $ normalize NFD "\x0FB2\x0334\x0F81"
["0FB2","0334","0F71","0F80"]

Note how 0F81 becomes 0F71 0F80

For now I've commented out these tests.

# Tailoring parsing

We still don't support

[suppressContractions [Nn]]

or

[reorder Cyrl]

