" StemmerST"
" Porter portuguese stemming algorithm in SmallTalk"
" Reference: http://snowball.tartarus.org/algorithms/portuguese/stemmer.html"

Object subclass: StemmerST [
  vowels := 'aeiouáéíóúâêô'.

  suffixes_1 := '(eza|ezas|ico|ica|icos|icas|ismo|ismos|ável|ível',
                '|ista|istas|oso|osa|osos|osas|amento|amentos|imento',
                '|imentos|adora|ador|aça~o|adoras|adores|aço~es|ante',
                '|antes|ância)$'.

  suffixes_2 := '(ada|ida|ia|aria|eria|iria|ará|ara|erá|era|irá|ava',
                '|asse|esse|isse|aste|este|iste|ei|arei|erei|irei|am',
                '|iam|ariam|eriam|iriam|aram|eram|iram|avam|em|arem',
                '|erem|irem|assem|essem|issem|ado|ido|ando|endo|indo',
                '|ara~o|era~o|ira~o|ar|er|ir|as|adas|idas|ias|arias',
                '|erias|irias|arás|aras|erás|eras|irás|avas|es|ardes',
                '|erdes|irdes|ares|eres|ires|asses|esses|isses|astes',
                '|estes|istes|is|ais|eis|íeis|aríeis|eríeis|iríeis',
                '|áreis|areis|éreis|ereis|íreis|ireis|ásseis|ésseis',
                '|ísseis|áveis|ados|idos|ámos|amos|íamos|aríamos',
                '|eríamos|iríamos|áramos|éramos|íramos|ávamos|emos',
                '|aremos|eremos|iremos|ássemos|êssemos|íssemos|imos',
                '|armos|ermos|irmos|eu|iu|ou|ira|iras)$'.

  replaceNasalisedVowels: word [
    |r|
    r := word.
    r := (r replacingRegex: 'ã' with: 'a~').
    r := (r replacingRegex: 'õ' with: 'o~').
    ^r
  ]

  returnNasalisedVowels: word [
    |r|
    r := word.
    r := (r replacingRegex: 'a~' with: 'ã').
    r := (r replacingRegex: 'o~' with: 'õ').
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
        do: [ r1 := '' ].
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

  "If the second letter is a consonant, RV is the region after the next following
  vowel, or if the first two letters are vowels, RV is the region after the next
  consonant, and otherwise (consonant-vowel case) RV is the region after the third
  letter. But RV is the end of the word if these positions cannot be found."
  getRV: word [
    |regex_a regex_b regex_c|

    " If the second letter is a consonant, RV is the region after the next following vowel"
    "regex_a := '(^[a-z', vowels, '][^', vowels, ']+[', vowels, '])'."
    regex_a := '(^[a-záé][^aeiou]+[aeiou])'.
    (word =~ regex_a) ifMatched: [ :match | ^ (word replacingRegex: ('^', (match at: 0)) with: '') ].

    " If the first two letters are vowels, RV is the region after the next consonant"
    regex_b := '^([', vowels, ']{2}[^', vowels, '])(.*)'.
    (word =~ regex_b) ifMatched: [ :match | ^ (word replacingRegex: ('^', (match at: 1)) with: '') ].

    " Consonant-vowel case RV is the region after the 3rd letter"
    regex_c := '^[^', vowels, '][', vowels, '].(.*)$'.
    (word =~ regex_c) ifMatched: [ :match | ^ match at: 1 ].

    " But RV is the end of the word if these positions cannot be found"
    " ??? "
    ^''
  ]

  doStep1: stemmed r1: rr1 r2: rr2 rv: rrv [
    |ret r1 r2 rv|
    ret := stemmed.
    r1 := rr1.
    r2 := rr2.
    rv := rrv.

    " delete suffixes_1 if in R2"
    r2 ifNotNil: [(r2 =~ suffixes_1) ifMatched: [ :m | ret := stemmed replacingRegex: ((m at: 1), '$') with: '' ]].

    " replace logia, logias with log if in R2"
    r2 ifNotNil: [(r2 =~ '(logia|logias)$') ifMatched: [ :m | ret := stemmed replacingRegex: ((m at: 1), '$') with: 'log' ]].

    " replace ución, uciones with u if in R2"
    r2 ifNotNil: [(r2 =~ '(ución|uciones)$') ifMatched: [ :m | ret := stemmed replacingRegex: ((m at: 1), '$') with: 'u' ]].

    " replace ência, ências with ente if in R2"
    r2 ifNotNil: [(r2 =~ '(ência|ências)$') ifMatched: [ :m | ret := stemmed replacingRegex: ((m at: 1), '$') with: 'ente' ]].

    " delete amente if in R1"
    r1 ifNotNil: [(r1 =~ '(amente)$') ifMatched: [ :m | ^ stemmed replacingRegex: ((m at: 1), '$') with: '' ]].
    r2 ifNotNil: [(r2 =~ '(iv).*(amente)$') ifMatched: [ :m | ret := stemmed replacingRegex: ((m at: 2), '$') with: '' ]].
    r2 ifNotNil: [(r2 =~ '(at).*(amente)$') ifMatched: [ :m | ret := stemmed replacingRegex: ((m at: 2), '$') with: '' ]].
    r2 ifNotNil: [(r2 =~ '(os|ic|ad)(amente)$') ifMatched: [ :m | ret := stemmed replacingRegex: ((m at: 2), '$') with: '' ]].

    " delete mente if in R2"
    "r2 ifNotNil: [(r2 =~ '(ante|avel|ível|)(mente)$') ifMatched: [ :m | ret := stemmed replacingRegex: ((m at: 1), '$') with: '' ]]."
    r2 ifNotNil: [(r2 =~ '(mente)$') ifMatched: [ :m | ret := stemmed replacingRegex: ((m at: 1), '$') with: '' ]].

    " idade, idades"
    r2 ifNotNil: [(r2 =~ '(abil|ic|iv|)(idade[s]*)$') ifMatched: [ :m | ret := stemmed replacingRegex: ((m at: 2), '$') with: '' ]].

    r2 ifNotNil: [(r2 =~ '(iva|ivo|ivas|ivos)$') ifMatched: [ :m | ret := stemmed replacingRegex: ((m at: 1), '$') with: '' ]].

    " replace with ir if in RV and preceded by e"
    rv ifNotNil: [(rv =~ 'e.*(ira[s]*)$') ifMatched: [ :m | ret := stemmed replacingRegex: ((m at: 1), '$') with: 'ir' ]].

    ^ret.
  ]

  replaceIfFound: word target: target regex: regex with: with [
    |ret|
    ret := ''.
    (target =~ regex)
      ifMatched:
        [ :m | ret := word replacingRegex: ((m at: 1), '$') with: with ].
    ^ret.
  ]

  stem: word [
    " Performs the stemming"
    <category: 'algorithmic'>
    |stemmed r1 r2 rv s1 s2 s3 original|
    s1 := false.
    s2 := false.
    s3 := false.
    original := (self replaceNasalisedVowels: word) asLowercase.
    stemmed := original.

    r1 := self getR1: stemmed.
    r2 := self getR2: stemmed.
    rv := self getRV: stemmed.

    stemmed := self doStep1: stemmed r1: r1 r2: r2 rv: rv.
    r1 := self getR1: stemmed.
    r2 := self getR2: stemmed.
    rv := self getRV: stemmed.

    (original ~= stemmed) ifTrue: [ s1 := true ].

    s1 ifFalse: [
      " Step 2: Verb suffixes"
      rv ifNotNil: [(rv =~ suffixes_2) ifMatched: [ :m | stemmed := stemmed replacingRegex: ((m at: 1), '$') with: '' ]].
      r1 := self getR1: stemmed.
      r2 := self getR2: stemmed.
      rv := self getRV: stemmed.
      (original ~= stemmed) ifTrue: [ s2 := true ].
    ].

    (s1 | s2) ifTrue: [
      " Step 3: Delete suffix i if in RV and preceded by c"
      rv ifNotNil: [(rv =~ 'c(i)$') ifMatched: [ :m | stemmed := stemmed replacingRegex: ((m at: 1), '$') with: '' ]].
      r1 := self getR1: stemmed.
      r2 := self getR2: stemmed.
      rv := self getRV: stemmed.
      (original ~= stemmed) ifTrue: [ s3 := true ].
    ].

    (s1 not & s2 not) ifTrue: [
      " Step 4: Residual suffix (only if S1 and S2 did not change)"
      rv ifNotNil: [(rv =~ '(os|a|i|o|á|í|ó)$') ifMatched: [ :m | stemmed := stemmed replacingRegex: ((m at: 1), '$') with: '' ]].
      r1 := self getR1: stemmed.
      r2 := self getR2: stemmed.
      rv := self getRV: stemmed.
    ].

    " Always do Step 5: "
    rv ifNotNil: [(rv =~ '(e|é|ê)$') ifMatched: [ :m | stemmed := stemmed replacingRegex: ((m at: 1), '$') with: '' ]].
    "rv ifNotNil: [(rv =~ '(gu|ci)(u|i)$') ifMatched: [ :m | stemmed := stemmed replacingRegex: ((m at: 2), '$') with: '' ]]."
    "rv ifNotNil: [(rv =~ '(gu)$') ifMatched: [ :m | stemmed := stemmed replacingRegex: 'u$' with: '' ]]."
    (stemmed =~ '(gu)$') ifMatched: [ :m |
      (rv =~ '(u)$') ifMatched: [ :m2 |
        stemmed := stemmed replacingRegex: ((m2 at: 1), '$') with: ''
      ]
    ].

    rv ifNotNil: [(rv =~ '(ç)$') ifMatched: [ :m | stemmed := stemmed replacingRegex: ((m at: 1), '$') with: 'c' ]].

    stemmed := self returnNasalisedVowels: stemmed.
    ^stemmed
  ]
]
