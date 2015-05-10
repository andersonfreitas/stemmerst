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
    r := (word replacingRegex: 'ã' with: 'a~').
    r := (word replacingRegex: 'õ' with: 'o~').
    ^r
  ]

  returnNasalisedVowels: word [
    |r| 
    r := (word replacingRegex: 'a~' with: 'ã').
    r := (word replacingRegex: 'o~' with: 'õ').
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
    regex_a := '^[a-z][^', vowels, ']+[', vowels, '](.*)'.
    (word =~ regex_a) ifMatched: [ :match | ^match at: 1 ].

    " If the first two letters are vowels, RV is the region after the next consonant"
    regex_b := '^[', vowels, ']{2}[^', vowels, '](.*)'.
    (word =~ regex_b) ifMatched: [ :match | ^ match at: 1 ].
  
    " Consonant-vowel case RV is the region after the 3rd letter"
    regex_c := '^[^', vowels, '][', vowels, '].(.*)$'.
    (word =~ regex_c) ifMatched: [ :match | ^ match at: 1 ].

    " But RV is the end of the word if these positions cannot be found"
    " ??? "
  ]

  doStep1: stemmed r1: r1 r2: r2 rv: rv [
    " delete suffixes_1 if in R2"
    r2 ifNotNil: [(r2 =~ suffixes_1) ifMatched: [ :m | ^ stemmed replacingRegex: ((m at: 1), '$') with: '' ]].

    " replace logia, logias with log if in R2"
    r2 ifNotNil: [(r2 =~ '(logia|logias)$') ifMatched: [ :m | ^ stemmed replacingRegex: ((m at: 1), '$') with: 'log' ]].

    " replace ución, uciones with u if in R2"
    r2 ifNotNil: [(r2 =~ '(ución|uciones)$') ifMatched: [ :m | ^ stemmed replacingRegex: ((m at: 1), '$') with: 'u' ]].

    " replace ência, ências with ente if in R2"
    r2 ifNotNil: [(r2 =~ '(ência|ências)$') ifMatched: [ :m | ^ stemmed replacingRegex: ((m at: 1), '$') with: 'ente' ]].

    " delete amente if in R1"
    r1 ifNotNil: [(r1 =~ '(amente)$') ifMatched: [ :m | ^ stemmed replacingRegex: ((m at: 1), '$') with: '' ]].
    r2 ifNotNil: [(r2 =~ 'iv(amente)$') ifMatched: [ :m | ^ stemmed replacingRegex: ((m at: 1), '$') with: '' ]].
    r2 ifNotNil: [(r2 =~ '[at]*(amente)$') ifMatched: [ :m | ^ stemmed replacingRegex: ((m at: 1), '$') with: '' ]].
    r2 ifNotNil: [(r2 =~ '[os|ic|ad](amente)$') ifMatched: [ :m | ^ stemmed replacingRegex: ((m at: 1), '$') with: '' ]].

    " delete mente if in R2"
    r2 ifNotNil: [(r2 =~ '[ante|avel|ível|](mente)$') ifMatched: [ :m | ^ stemmed replacingRegex: ((m at: 1), '$') with: '' ]].

    " idade, idades"
    r2 ifNotNil: [(r2 =~ '[abil|ic|iv|](idade[s]*)$') ifMatched: [ :m | ^ stemmed replacingRegex: ((m at: 1), '$') with: '' ]].

    r2 ifNotNil: [(r2 =~ '[at]*([iva|ivo][s]*)$') ifMatched: [ :m | ^ stemmed replacingRegex: ((m at: 1), '$') with: '' ]].


    " replace with ir if in RV and preceded by e"
    rv ifNotNil: [(rv =~ 'e.*(ira[s]*)$') ifMatched: [ :m | ^ stemmed replacingRegex: ((m at: 1), '$') with: 'ir' ]].
  ]

  stem: word [
    " Performs the stemming"
    <category: 'algorithmic'>
    |stemmed r1 r2 rv|
    stemmed := (self replaceNasalisedVowels: word) asLowercase.

    r1 := self getR1: stemmed.
    r2 := self getR2: stemmed.
    rv := self getRV: stemmed.

    stemmed := self doStep1: stemmed r1: r1 r2: r2 rv: rv.

    " Step 2: Verb suffixes"
    rv ifNotNil: [(rv =~ suffixes_2) ifMatched: [ :m | stemmed := stemmed replacingRegex: ((m at: 1), '$') with: '' ]].
    
    " Step 3: Delete suffix i if in RV and preceded by c"
    rv ifNotNil: [(rv =~ '(ci)$') ifMatched: [ :m | stemmed := stemmed replacingRegex: ((m at: 1), '$') with: '' ]].

    " Step 4: Residual suffix"
    rv ifNotNil: [(rv =~ '(os|a|i|o|á|í|ó)$') ifMatched: [ :m | stemmed := stemmed replacingRegex: ((m at: 1), '$') with: '' ]].
 
    " Step 5: "
    rv ifNotNil: [(rv =~ '(e|é|ê)$') ifMatched: [ :m | stemmed := stemmed replacingRegex: ((m at: 1), '$') with: '' ]].
    rv ifNotNil: [(rv =~ '[gu|ci](u|i)$') ifMatched: [ :m | stemmed := stemmed replacingRegex: ((m at: 1), '$') with: '' ]].
    rv ifNotNil: [(rv =~ '(ç)$') ifMatched: [ :m | stemmed := stemmed replacingRegex: ((m at: 1), '$') with: '' ]].

    stemmed := self returnNasalisedVowels: stemmed.
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
    self expect: (stemmer stem: 'financiamentos')  toEqual: 'financi'.
    self expect: (stemmer stem: 'parapsicologia')  toEqual: 'parapsicolog'.
    self expect: (stemmer stem: 'carinhosamente')  toEqual: 'carinhos'.
    self expect: (stemmer stem: 'respectivamente') toEqual: 'respectiv'.
    self expect: (stemmer stem: 'exatamente')      toEqual: 'exat'.
    self expect: (stemmer stem: 'lamentávelmente') toEqual: 'lamentável'.
    self expect: (stemmer stem: 'fiandeiras')      toEqual: 'fiandeir'.
  ]

  testRV [
    self expect: (stemmer getRV: 'macho')       toEqual: 'ho'.
    self expect: (stemmer getRV: 'oliva')       toEqual: 'va'.
    self expect: (stemmer getRV: 'trabalho')    toEqual: 'balho'.
    self expect: (stemmer getRV: 'aureo')       toEqual: 'eo'.
    self expect: (stemmer getRV: 'fiandeiras')  toEqual: 'ndeiras'.

    " TODO: O Smalltalk não está reconhecendo os caracteres acentuados."
    "self expect: (stemmer getRV: 'áureo')      toEqual: 'eo'." 
  ]

  expect: a toEqual: b [
    self assert: a = b description: 'Expected <', a, '> to equal <', b, '>'.
  ]
]

StemmerTestCase buildSuiteFromLocalSelectors run printNl!
"(StemmerTestCase new) setUp; testForWordRegions; tearDown!"
"(StemmerTestCase new) setUp; testRV; tearDown!"

