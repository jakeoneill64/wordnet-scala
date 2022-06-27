# Wordnet - Scala

A scala version of the lexicographical relatedness API.

To use the api, a synsets and hypernyms file are required as may be shown for example 
<br>https://coursera.cs.princeton.edu/algs4/assignments/wordnet/files/synsets.txt
<br>https://coursera.cs.princeton.edu/algs4/assignments/wordnet/files/hypernyms.txt

To use the API, provide a URL or path to the files as constructor args to a Wordnet object.

_for example_

```
val wordNet = new WordNet(
  "https://coursera.cs.princeton.edu/algs4/assignments/wordnet/files/synsets.txt",
  "https://coursera.cs.princeton.edu/algs4/assignments/wordnet/files/hypernyms.txt"
)
val shortestPath = wordNet.sap("miracle", "action")
```


