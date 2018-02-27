#!/opt/smalltalk/bin/gst -f
" #!/afs/cats.ucsc.edu/courses/cmps112-wm/usr/smalltalk/bin/gst -f "

"

  Wyatt Ades and Lee White
  hzip.st

"


"
  Globals
"
nl := Character nl.
Smalltalk at: #compareChar put: false.

"
  class ZeroArray
  from Mackey examples
"
Object subclass: ZeroArray [
   | theArray |

   ZeroArray class >> new [
      self shouldNotImplement.
   ]

   ZeroArray class >> new: size [
      | result |
      result := super new.
      result init: size.
      ^ result
   ]

   init: size [
      theArray := Array new: size.
   ]

   size [
      ^ theArray size.
   ]

   at: index [
      ^ theArray at: (index + 1).
   ]

   at: index put: value [
      ^ theArray at: index + 1 put: value.
   ]

   incr: index [
      (self at: index) isNil
         ifTrue: [ self at: index put: 0].
      self at: index put: (self at: index) + 1.
   ]

   keysAndValuesDo: aBlock [
      (0 to: (self size) - 1) do: [ :index |
         aBlock value: index value: (self at: index).
      ]
   ]
]

"
  class BitStream
"
Object subclass: BitStream [
  | filename readNotWrite fileStream bitindex currentByte |

  BitStream class >> new: iFilename readNotWrite: iReadNotWrite [
    |result|
    result := self new.
    result init: iFilename readNotWrite: iReadNotWrite.
    ^result.
  ]

  init: iFilename readNotWrite: iReadNotWrite [
    filename := iFilename.  
    readNotWrite := iReadNotWrite.
    self reset.
  ]

  reset [
    fileStream isNil ifFalse: [ self close. ].

    bitindex := 7.
    currentByte := 0.

    readNotWrite ifTrue: [
      fileStream := FileStream open: filename mode: FileStream read.
    ] ifFalse: [
      filename isNil ifTrue: [
        fileStream := stdout.
      ] ifFalse: [
        fileStream := FileStream open: filename mode: FileStream create.
      ].
    ].
  ]

  readBit [
    | bit |
    bitindex = 7 ifTrue: [ currentByte := self readByte. ].
    bit := currentByte bitAt: (bitindex + 1).
    bitindex := bitindex - 1.
    bitindex < 0 ifTrue: [ bitindex := 7. ].
    ^ bit
  ]

  readByte [
    | newByte res |

    newByte := fileStream nextByte.

    bitindex = 7 ifTrue: [
      res := newByte.
    ] ifFalse: [
      res := ((currentByte bitShift: (7 - bitindex)) bitAnd: 16rFF)
          bitOr: (newByte bitShift: (-1 - bitindex)).

      currentByte := newByte.
    ].

    ^ res
  ]

  writeBits: bits [
    | bit |
    1 to: bits size do: [ :i |
      bit := (bits byteAt: i) - 48.
      
      " TEMP "
      (bit < 0) | (bit > 1) ifTrue: [self error: 'NOOOOOOO'.].
      
      currentByte := currentByte bitAt: (bitindex + 1) put: bit.
      bitindex := bitindex - 1.
      bitindex < 0 ifTrue: [
        self writeByte: currentByte.
        currentByte := 0.
        bitindex := 7.
      ].
    ].
  ]

  writeByte: byte [
    fileStream nextPutByte: byte.
  ]

  writeString: string [
    fileStream << string << Character nl.
  ]

  flush [
    bitindex ~= 7 ifTrue: [ self writeByte: currentByte. ].
  ]

  atEnd [
    ^ (bitindex = 7) & (fileStream atEnd)
  ]

  close [
    readNotWrite ifFalse: [
      self flush.
    ].
    filename isNil ifFalse: [
      fileStream close.
    ].
  ]
]

"
  class Tree
"
Object subclass: Tree [
  | char count bitpath lnode rnode |
  char [ ^ char ]
  count [ ^ count ]
  bitpath [ ^ bitpath ]
  lnode [ ^ lnode ]
  rnode [ ^ rnode ]

  Tree class >> new: iChar count: iCount [
    | result |
    result := self new.
    result init: iChar count: iCount.
    ^ result
  ]

  init: iChar count: iCount [
    char := iChar.
    count := iCount.
  ]

  setLeft: aLeft andRight: aRight [
    lnode := aLeft.
    rnode := aRight.
  ]

  setBitpath: aPath [
    bitpath := aPath.
  ]

  <= other [
    compareChar | (count = other count) 
      ifTrue: [ ^ char < other char ]
      ifFalse: [ ^ count < other count ].
  ]
]

"
  returns encoding table
"
getEncodingTable := [ :tree |
  | collec recurse |

  Smalltalk at: #compareChar put: true.

  collec := SortedCollection new.

  recurse := [ :node :bitpath |
    node lnode isNil ifFalse: [
      recurse value: node lnode value: (bitpath , '0').
    ].
    node rnode isNil ifFalse: [
      recurse value: node rnode value: (bitpath , '1').
    ].

    (node lnode isNil) & (node rnode isNil) ifTrue: [
      node setBitpath: bitpath.
      collec add: node.
    ].
  ].


  recurse value: tree value: ''.

  collec
].

"
  write file encoding to stream using postOrder traversel
"
postOrderWrite := [ :tree :stream |
  | recurse |

  recurse := [ :node |
    node lnode isNil ifFalse: [
      recurse value: node lnode.
    ].
    node rnode isNil ifFalse: [
      recurse value: node rnode.
    ].

    (node lnode isNil) & (node rnode isNil) ifTrue: [
      (node char = 0) | (node char = 256) ifTrue: [
        node char = 0 ifTrue: [
          stream writeBits: '0000000000'.
        ] ifFalse: [
          stream writeBits: '0000000001'.
        ].
      ] ifFalse: [
        stream writeBits: ('0' ,
            (node char printPaddedWith: $0 to: 8 base: 2)).
      ]
    ] ifFalse: [
      stream writeBits: '1'.
    ].
  ].

  recurse value: tree.
].

"
  compress file
"
compress := [ :readStream :writeStream :printTree |
  | byte encodingTable encodingMap tree freq collec |

  freq := ZeroArray new: 257.

  [ readStream atEnd ] whileFalse: [
    byte := readStream readByte.
    freq incr: byte.
  ].
  freq at: 256 put: 1.

  collec := SortedCollection new.

  freq keysAndValuesDo: [ :i :count |
    count isNil not ifTrue: [
      collec add: (Tree new: i count: count).
    ].
  ].

  [ collec size > 1 ] whileTrue: [
    | left right branch |

    left := collec first.
    collec removeFirst.

    right := collec first.
    collec removeFirst.

    branch := Tree new: left char count: (left count + right count).
    branch setLeft: left andRight: right.

    collec add: branch.
  ].

  tree := collec first.
  collec removeFirst.

  encodingTable := getEncodingTable value: tree.
  encodingTable beConsistent.

  printTree ifTrue: [

    encodingTable do: [ :node |
      | head |

      node char = 256 ifTrue: [
        head := 'EOF'.
      ] ifFalse: [
        "printable range [33,126]"
        ((node char >= 33) & (node char <= 126)) ifTrue: [
          head := ' ' , node char asCharacter asString  , ' '.
        ] ifFalse: [
          head := 'x' , (node char printPaddedWith: $0 to: 2 base: 16).
        ].
      ].

      writeStream writeString: (
        head , 
        (node count printPaddedWith: Character space to: 8 base: 10) , 
        '  ' , (node bitpath)
      ).
    ].
  ] ifFalse: [
    readStream reset.

    readStream atEnd ifFalse: [

      postOrderWrite value: tree value: writeStream.

      " would'nt dare forget this 1! "
      writeStream writeBits: '1'.

      encodingMap := ZeroArray new: 257.
      encodingTable do: [:node |
        encodingMap at: (node char) put: (node bitpath).
      ].

      [ readStream atEnd ] whileFalse: [
        |bits|
        bits := encodingMap at: (readStream readByte).
        writeStream writeBits: bits.
      ].
      writeStream writeBits: (encodingMap at: 256).
    ].
  ].
].

"
  uncompress file
"
uncompress := [ :readStream :writeStream |
  | stack isLeaf tree node stop char |

  readStream atEnd ifFalse: [

    stack := OrderedCollection new.

    stop := false.
    [ (readStream atEnd) | stop] whileFalse: [
      isLeaf := readStream readBit.

      isLeaf = 0 ifTrue: [
        char := readStream readByte.

        char = 0 ifTrue: [
          | extraBit |
          extraBit := readStream readBit.
          extraBit = 1 ifTrue: [ char := 256. ].
        ].

        stack addLast: (Tree new: char count: nil).
      ] ifFalse: [
        stack size = 1 ifFalse: [
          | left right branch |
          right := stack last.
          stack removeLast.

          left := stack last.
          stack removeLast.

          branch := Tree new: nil count: nil.
          branch setLeft: left andRight: right.
          stack addLast: branch.
        ] ifTrue: [
          stop := true.
        ].
      ].
    ].

    tree := stack last.
    node := tree.

    stop := false.
    [ (readStream atEnd) | stop ] whileFalse: [
      | bit |
      bit := readStream readBit.

      bit = 0 ifTrue: [
        node := node lnode.
      ] ifFalse: [
        node := node rnode.
      ].

      (node lnode isNil) & (node rnode isNil) ifTrue: [
        node char = 256 ifTrue: [
          stop := true.
        ] ifFalse: [
          writeStream writeByte: node char.
          node := tree.
        ].
      ].
    ].
  ].
].


"
  Main
"

| mode inputfile outputfile readStream writeStream |

[

  " Parse arguments "
  Smalltalk arguments: '-d -c -t -u'
  do: [:opt :arg |
    arg isNil ifTrue: [
      opt ~= $d ifTrue: [
        mode isNil ifTrue: [
          mode := opt.
        ] ifFalse: [
          Exception signal: 'must provide only one option'.
        ].
      ].
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

  inputfile isNil ifTrue: [
    Exception signal: 'must provide inputfile'.
  ].

  mode isNil ifTrue: [
    Exception signal: 'must provide one option'.
  ].

] on: Exception do: [ :err |
  " Print parsing error, print usage, and exit "
  stderr << 'Invald args error: ' << err messageText << nl
    << 'Usage: -dtcu inputfile [outputfile]' << nl.
  ObjectMemory quit: 1.
].

" open read and write streams, and catch file errors "
[
  readStream := BitStream new: inputfile readNotWrite: true.
  writeStream := BitStream new: outputfile readNotWrite: false.
] on: SystemExceptions.FileError do: [ :err |
  " Print file open error and exit "
  stderr << 'File Error: ' << err messageText << nl.
  ObjectMemory quit: 1.
].

" [ "

  " compress, decompress, or print tree depending on given args "
  mode = $u ifTrue: [
    uncompress value: readStream value: writeStream.
  ] ifFalse: [
    compress value: readStream value: writeStream value: (mode = $t).
  ].
" ] on: Exception do: [ :err |
  stderr << 'Fatal Error: ' << err messageText << nl.
  ObjectMemory quit: 1.
]. "

" close file streams "
readStream close.
writeStream close.
