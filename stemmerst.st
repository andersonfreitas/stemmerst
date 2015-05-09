" StemmerST"
" Porter portuguese stemming algorithm in SmallTalk"
" Reference: http://snowball.tartarus.org/algorithms/portuguese/stemmer.html"

Object subclass: StemmerST [
  "Vowels:"
    "aeiou"
    "áéíóú"
    "âê ô "

  normalizeNasalidedVowel: word [
    |r| 
    r := (word replacingRegex: 'ã' with: 'a~').
    r := (word replacingRegex: 'õ' with: 'o~').
    ^r
  ]

  stem: word [
    |w|
    "((string =~ 'ee') ifMatched: ['match!'])"

    w := (self normalizeNasalidedVowel: word).
    ^w
  ]
]

stemmer := StemmerST new.
(stemmer stem: 'doações') printNl.


