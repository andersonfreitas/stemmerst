" StemmerST"
" Porter portuguese stemming algorithm in SmallTalk"
" Reference: http://snowball.tartarus.org/algorithms/portuguese/stemmer.html"

Object subclass: StemmerST [

  stem: word [
  
    ((string =~ 'ee') ifMatched: ['match!'])

    ^'teste'
  ]

]

stemmer := StemmerST new.
(stemmer stem: 'doações') printNl.

