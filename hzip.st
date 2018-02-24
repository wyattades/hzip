#!/afs/cats.ucsc.edu/courses/cmps112-wm/usr/smalltalk/bin/gst -f
"#!/opt/smalltalk/bin/gst -f"


"

  Wyatt Ades and Lee White
  hzip.st

"


" Globals "
nl := Character nl.
inputfile := nil.
outputfile := nil.
debug := false.
compress := false.
tree := false.
uncompress := false.
Smalltalk at:#compareChar put: false.

Object subclass: BitStream [
   |bitIndex byte myStream|
   BitStream class >> new: fileStream [
      |result|
      result := super new.
      result init: fileStream.
      ^result
   ]
   init: fileStream [
      myStream := fileStream.
      bitIndex := 1.
   ]
   nextBit [
      bitIndex = 1 ifTrue: [
         byte := myStream next.
         bitIndex := 9.
      ].
      bitIndex := bitIndex - 1.
      ^byte value bitAt: bitIndex
   ]
   atEnd [
      ^bitIndex = 1 and: [myStream atEnd]
   ]
   currByte [
      ^byte
   ]
]

Object subclass: Tree [
  |char count rnode lnode bitpath|
  char [ ^ char ]
  count [ ^ count ]
  rnode [ ^ rnode ]
  lnode [ ^ lnode ]
  bitpath [ ^ bitpath ]

  Tree class >> new: aChar count: aCount [
    |result|
    result := super new.
    result setChar: aChar andCount: aCount.
    ^result
  ]

  setLeft: aLeft andRight: aRight [
    lnode := aLeft.
    rnode := aRight.
  ]

  setBPath: aPath [
    bitpath := aPath.
  ]

  compare: b [
    (compareChar) | (count = b count) isTrue: [ ^ (char > b char) ] isFalse: [ ^ (count > b count) ].
  ]

  getPaths [
    |collec getPathsRecur|

    Smalltalk at: #compareChar put: true.

    collec := SortedCollection new.
    collec sortBlock: [ :a :b | a compare: b ].

    getPathsRecur := [ :node :bitpath |
      node lnode isNil isFalse: [
        getPathsRecur node: node lnode bitpath: bitpath , '0'.
      ].
      node rnode isNil isFalse: [
        getPathsRecur node: node rnode bitpath: bitpath , '1'.
      ].
      node lnode isNil & node rnode isNil isTrue: [
        node setBPath: bitpath.
        collec add: node.
      ].
    ].

    getPathsRecur node: self bitpath: ''.


  ]

 postOrder: stream currNode: node [
     node lnode isNil isFalse: [ lnode postOrder: stream currNode: node lnode ]
     node rnode isNil isFalse: [ rnode postOrder: stream currNode: node rnode ]
 
     node lnode isNil & node rnode isNil isTrue: [
       (node char = 0) | (node char = 256) isTrue: [
         stream := stream, '000000000'.
         (node char = 0) isTrue: [ stream := stream, '0'. ] isFalse: [ stream := stream, '1'. ]
       ] ifFalse: [
         stream := stream, '0', (node char value
       ]
     ]
   ]
]

"Object subclass: Zipper [
  "|freqTable|"

  compress: data isT: printTree fstream: stream [
    freqTable := Array new: 257.

    collec := Sorted Collection new.
    1 to: 257 do: [x: | 
      count := freqTable at: x.
      count > 0 isTrue: [  ]


    ]
  ]

]
"


FileStream extend [
  |data|
  testRead [
    data := ''.
    [ self atEnd ] whileFalse: [
      data := data , self nextLine , Character nl asString.
    ].
    ^data
  ]

  testWrite: data [ 
    self nextPutAll: data.
  ]
].

" Parse arguments "
[
  Smalltalk arguments: '-d -c -t -u'
  do: [:opt :arg |
    arg = nil ifTrue: [
      opt = $d ifTrue: [ debug := true. ].
      opt = $c ifTrue: [ compress := true. ].
      opt = $t ifTrue: [ tree := true. ].
      opt = $u ifTrue: [ uncompress := true. ].
    ] ifFalse: [
      inputfile isNil ifTrue: [
        inputfile := arg.
      ] ifFalse: [
        outputfile isNil ifTrue: [
          outputfile := arg.
        ] ifFalse: [
          Exception signal: 'too many args'.
        ].
      ].
    ].
  ].
  
  inputfile isNil ifTrue:[
    Exception signal: 'must provide inputfile'.
  ].

  ((compress & uncompress) |
  (compress & tree) | (tree & uncompress)) ifTrue: [
    Exception signal: 'must provide only one option'.
  ].

  compress ifTrue: [

  ] ifFalse: [
    uncompress ifTrue: [

    ] ifFalse: [
      tree ifTrue: [

      ] ifFalse:[
        Exception signal: 'must provide one option'
      ]
    ]
  ]
] on: Exception do: [ :err |
  " Print parsing error, print usage, and exit "
  stderr << 'Error: ' << err messageText << nl
    << 'Usage: -dtcu inputfile [outputfile]' << nl.
  ObjectMemory quit: 1.
].

[
  | data |
  input := FileStream open: inputfile mode: FileStream read.
  data := input testRead.
  input close.

  " outputfile := stdout. "
  output := FileStream open: outputfile mode: FileStream create.
  output testWrite: data.
  output close.
] on: SystemExceptions.FileError do: [:err |
  " Print file open error and exit "
  stderr << 'Error: ' << err messageText << nl.
  ObjectMemory quit: 1.
].


" Exit success "
ObjectMemory quit: 0.
