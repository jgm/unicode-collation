# Incremental normalization

Pending unicode-transforms implementing this,
we could try implementing it ourselves.

http://www.unicode.org/Public/5.1.0/ucd/UCD.html#UnicodeData.txt

https://www.unicode.org/Public/13.0.0/ucd/UnicodeData.txt

https://unicode.org/reports/tr15/#Norm_Forms

It doesn't seem too complicated.

Take code points.
For each code point, decompose it using the
Decomposition_Mapping in UnicodeDate.  (Must
do this recursively.)
Once fully decomposed, rearrange combining marks
(marks with Canonical_Combining_Class > 0) in
ascending order of ccc...

If we do this incrementally, we have to decide
when we can safely stop:
We can safely omit a stream of code points
when the next code point has ccc == 0.

Also, implement quick check for NFD form?
check:
- code point not decomposable
- if ccc > 0, then the following code point either
  has >= ccc or ccc == 0.

