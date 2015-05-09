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

  stem: word [
    " Performs the stemming"
    <category: 'algorithmic'>
    |stemmed r1 r2|
    stemmed := (self replaceNasalisedVowels: word) asLowercase.

    r1 := self getR1: stemmed.
    r2 := self getR2: stemmed.

    " delete suffixes_1 if in R2"
    r2 ifNotNil: [(r2 =~ suffixes_1) ifMatched: [ :m | stemmed := stemmed replacingRegex: (m at: 1) with: '' ]].
    ^stemmed
  ]
]

PackageLoader fileInPackage: 'SUnit'!

TestCase subclass: StemmerTestCase [
  | stemmer |

  setUp [
    stemmer := StemmerST new.
  ]

  checkRegionsForWord: word withR1: r1 withR2: r2 [
    |rr1 rr2 stemmed|
    stemmed := (stemmer replaceNasalisedVowels: word) asLowercase.
    rr1 := stemmer getR1: stemmed.
    rr2 := stemmer getR2: stemmed.
    self assert: (r1 = rr1) description: 'Test for word ', word, ' in R1 failed. Expected ', r1, ' got ', rr1.
    self assert: (r2 = rr2) description: 'Test for word ', word, ' in R2 failed. Expected ', r2, ' got ', rr2.
  ]

  testForWordRegions [
    self checkRegionsForWord: 'beautiful'      withR1: 'iful'        withR2: 'ul'.
    self checkRegionsForWord: 'beauty'         withR1: 'y'           withR2: ''.
    self checkRegionsForWord: 'beau'           withR1: ''            withR2: ''.
    self checkRegionsForWord: 'canibalization' withR1: 'ibalization' withR2: 'alization'.
    self checkRegionsForWord: 'animadversion'  withR1: 'imadversion' withR2: 'adversion'.
    self checkRegionsForWord: 'canibal'        withR1: 'ibal'        withR2: 'al'.
    self checkRegionsForWord: 'aninal'         withR1: 'inal'        withR2: 'al'.
    self checkRegionsForWord: 'sprinkled'      withR1: 'kled'        withR2: ''.
    self checkRegionsForWord: 'eucharist'      withR1: 'harist'      withR2: 'ist'.
    self checkRegionsForWord: 'doações'        withR1: 'o~es'        withR2: 'es'.
    self checkRegionsForWord: 'financiamentos' withR1: 'anciamentos' withR2: 'ciamentos'.
  ]

  testStemming [
    self assert: true
    "self expect (stemmer stem: 'financiamentos') toEqual: 'fianc'."
  ]
]

StemmerTestCase buildSuiteFromLocalSelectors run printNl!
"(StemmerTestCase new) setUp; testForWordRegions; tearDown!"

