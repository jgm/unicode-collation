# Lang

change to using a Trie for lookup?  see
the section on Lookup in https://tools.ietf.org/html/bcp47

define ToJSON/FromJSON etc?
We'll need this when we propagate this up to citeproc and
pandoc.

# Tailoring parsing

Handle / and | in tailorings!

We still don't support

e.g. [reorder Cyrl]

# Weird test failures with tibetan

 toHex $ normalize NFD "\x0FB2\x0334\x0F81"
["0FB2","0334","0F71","0F80"]

Note how 0F81 becomes 0F71 0F80

For now I've commented out these tests.

