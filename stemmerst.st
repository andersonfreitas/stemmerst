" StemmerST"
" Porter portuguese stemming algorithm in SmallTalk"
" Reference: http://snowball.tartarus.org/algorithms/portuguese/stemmer.html"

Object subclass: StemmerST [
  vowels := 'aeiouáéíóúâêô'.

  replaceNasalisedVowels: word [
    |r| 
    r := (word replacingRegex: 'ã' with: 'a~').
    r := (word replacingRegex: 'õ' with: 'o~').
    ^r
  ]
 
  "Region 1 (R1) is the region after the first non-vowel following a vowel, or is"
  "the null region at the end of the word if there is no such non-vowel"
  getR1: word [
    |r1|
    "beaut|iful"
    "beaut|y"
    "an|imal"
  
    [[ r1 := word =~ ('[', vowels, '][^', vowels, '](.*)') at: 1 ]
      on: SystemExceptions.IndexOutOfRange
      do: [ :e | e return: nil ]]
      on: SystemExceptions.ShouldNotImplement
      do: [ :e | e return: nil].

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
    <category: 'algorithm'>
    |w r1 r2|
    w := (self replaceNasalisedVowels: word).

    r1 := self getR1: w.
    r2 := self getR2: w.

    word printNl.
    (' - r1 = ', r1) printNl.
    (' - r2 = ', r2) printNl.

    "((string =~ 'ee') ifMatched: ['match!'])"

    ^w
  ]
]

stemmer := StemmerST new.
(stemmer stem: 'beautiful').
(stemmer stem: 'doações').
(stemmer stem: 'beauty').
(stemmer stem: 'canibal').
(stemmer stem: 'aninal').
(stemmer stem: 'canibalization').

