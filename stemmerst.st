" StemmerST"
" Porter portuguese stemming algorithm in SmallTalk"
" Reference: http://snowball.tartarus.org/algorithms/portuguese/stemmer.html"

Object subclass: StemmerST [
  vowels := 'aeiouáéíóúâêô'.

  suffixes_1 := '(eza|ezas|ico|ica|icos|icas|ismo|ismos|ável|ível|ista|istas|oso|osa|osos|osas|amento|amentos|imento|imentos|adora|ador|aça~o|adoras|adores|aço~es|ante|antes|ância)$'.
  
  replaceNasalisedVowels: word [
    |r| 
    r := (word replacingRegex: 'ã' with: 'a~').
    r := (word replacingRegex: 'õ' with: 'o~').
    ^r
  ]
 
  "Region 1 (R1) is the region after the first non-vowel following a vowel, or is"
  "the null region at the end of the word if there is no such non-vowel"
  getR1: word [
    |r1 regex|
    regex := '[', vowels, '][^', vowels, '](.*)'.
    [r1 := ((word =~ regex) 
      ifMatched: [ :match | match at: 1 ] 
      ifNotMatched: [''])] 
        on: SystemExceptions.IndexOutOfRange
        do: [ :e | e return: '' ].
    ^r1
  ]

  "R2 is the region after the first non-vowel following a vowel in R1, or is"
  "the null region at the end of the word if there is no such non-vowel"
  getR2: word [
    |r2|
    r2 := self getR1: word.
    r2 := self getR1: r2.
    ^r2
  ]

  stem: word [
    " Performs the stemming"
    <category: 'algorithmic'>
    |stemmed r1 r2|
    stemmed := (self replaceNasalisedVowels: word).

    r1 := self getR1: stemmed.
    r2 := self getR2: stemmed.

    word printNl.
    (' - r1 = ', r1) printNl.
    (' - r2 = ', r2) printNl.

    " delete suffixes_1 if in R2"
    r2 ifNotNil: [(r2 =~ suffixes_1) ifMatched: [ :m | stemmed := stemmed replacingRegex: (m at: 1) with: '' ]].
    ('== stemmed := ', stemmed) printNl.
    (String with: Character cr) printNl.
    ^stemmed
  ]
]

stemmer := StemmerST new.
(stemmer stem: 'beautiful').
(stemmer stem: 'doações').
(stemmer stem: 'beauty').
(stemmer stem: 'canibal').
(stemmer stem: 'aninal').
(stemmer stem: 'canibalization').
(stemmer stem: 'beau').
(stemmer stem: 'animadversion').
(stemmer stem: 'sprinkled').
(stemmer stem: 'eucharist').
(stemmer stem: 'financiamentos').

