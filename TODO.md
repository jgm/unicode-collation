# Lang

extension keywords case-insensitive?
maybe more? check.

change to using a Trie for lookup?  see
the section on Lookup in https://tools.ietf.org/html/bcp47

define ToJSON/FromJSON etc?
We'll need this when we propagate this up to citeproc and
pandoc.

# Tailoring parsing

Handle / and | in tailorings!

e.g. in fi traditional
```
                                        &T<<þ/h<<<Þ/h
```

e.g. in ja
```
<<<ぁ|ー=あ|ー=か|ー=ゕ
```

```
a <<< a | '-'
```
can turn into
```
a'-' = aa
```

and

```
a < z / e
```
is roughly
```
ae < z
```

We still don't support

e.g. [reorder Cyrl]

# Weird test failures with tibetan

 toHex $ normalize NFD "\x0FB2\x0334\x0F81"
["0FB2","0334","0F71","0F80"]

Note how 0F81 becomes 0F71 0F80

For now I've commented out these tests.

