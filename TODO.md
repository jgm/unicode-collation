# Tailorings

handle overrideCJK?  how does it work?

# Weird test failures with tibetan

 toHex $ normalize NFD "\x0FB2\x0334\x0F81"
["0FB2","0334","0F71","0F80"]

Note how 0F81 becomes 0F71 0F80

For now I've commented out these tests.

