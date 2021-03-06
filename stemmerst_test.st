FileStream fileIn: 'stemmerst.st' !
PackageLoader fileInPackage: 'SUnit' !

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
    self expect: (stemmer stem: 'financiamentos')  toEqual: 'financ'.
    self expect: (stemmer stem: 'neuropsicologia') toEqual: 'neuropsicolog'.
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
    self expect: (stemmer getRV: 'acidas')      toEqual: 'das'.

    " TODO: O Smalltalk não está reconhecendo os caracteres acentuados."
    "self expect: (stemmer getRV: 'áureo')      toEqual: 'eo'."
  ]

  ignoretestFullVocab [
    |file lines diffs i|
    i := 0.
    file := (File name: 'fixtures/diffs.txt') readStream.
    diffs := file lines collect: [ :line | line subStrings ].
    diffs do: [ :term |
      |a b|
      a := stemmer stem: term first.
      b := term second.
      i := i + 1.
      self assert: a = b
           description: '(', i asString,') Term: ', term first, '. Expected <', a, '> to equal <', b, '>'
           resumable: false.
    ].
  ]

  expect: a toEqual: b [
    self assert: a = b description: 'Expected <', a, '> to equal <', b, '>'.
  ]
]

StemmerTestCase buildSuiteFromLocalSelectors run printNl!
"(StemmerTestCase new) setUp; testFullVocab; tearDown!"
"(StemmerTestCase new) setUp; testRV; tearDown!"
