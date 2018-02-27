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
  class BitStream
"
Object subclass: BitStream [
  | filename readNotWrite fileStream bitindex byte |

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
    byte := 0.

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
    |bit|
    bitindex = 7 ifTrue: [ byte := self readByte. ].
    "dont know what byte.isbitset is"
    "confirmed"
    bit := 0.
    bitindex := bitindex - 1.
    bitindex < 0 ifTrue: [ bitindex = 7. ].
    ^bit
  ]

  readByte [
    ^0000
"    |chunk newbyte res|
    chunk := fileStream next.

    bitindex = 7 ifTrue: [
      ^newbyte
    ] ifFalse: [
      ??
      res = ((this.newbyte << (7 - this.bitindex)) & 0xFF) |
          (newbyte >> (this.bitindex + 1)).

      byte = newbyte.
      ^res"
    "]."
  ]

  writeByte: newByte [

  ]

  writeString: bits [

  ]

  flush [
    bitindex ~= 7 ifTrue: [ self writeByte byte. ].
  ]

  atEnd [
    ^ fileStream atEnd
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
    |result|
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
      ifTrue: [ ^ char > other char ]
      ifFalse: [ ^ count > other count ].
  ]
]

"
  returns encoding table
"
getEncodingTable := [ :tree |
  | collec recurse |

  Smalltalk at: #compareChar put: true.

  collec := SortedCollection new.
  " need this????: collec sortBlock: [ :a :b | a compare: b ]. "

  recurse := [ :node :bitpath |
    node lnode isNil ifFalse: [
      recurse node: node lnode bitpath: bitpath , '0'.
    ].
    node rnode isNil ifFalse: [
      recurse node: node rnode bitpath: bitpath , '1'.
    ].

    node lnode isNil & node rnode isNil ifTrue: [
      node setBitpath: bitpath.
      collec add: node.
    ].
  ].

  recurse node: tree bitpath: ''.

  ^collec
].

"
  write file encoding to stream using postOrder traversel
"
postOrderWrite := [ :tree :stream |
  | recurse |
  recurse := [ :node |
    node lnode isNil ifFalse: [
      recurse node: node lnode.
    ].
    node rnode isNil ifFalse: [
      recurse node: node rnode.
    ].

    node lnode isNil & node rnode isNil ifTrue: [
      node char = 0 || node char = 256 ifTrue: [
        stream nextPutAll: '000000000'.
        node char = 0 ifTrue: [
          stream nextPut: '0'.
        ] ifFalse: [
          stream nextPut: '1'.
        ].
      ] ifFalse: [
        stream nextPut: '0'.
        node char printOn: stream base: 2.
      ]
    ] ifFalse: [
      stream nextPut: '1'.
    ].
  ].

  recurse node: tree.
].

"
  compress file
"
compress := [ :readStream :writeStream :printTree |
  | byte encodingTable encodingMap tree freq collec |

  freq := Array new: 257.

  [ readStream atEnd ] whileFalse: [
    byte := readStream nextByte.
    freq at: byte put: (freq at: byte) + 1.
  ].

  freq at: 256 put: 1.

  collec := SortedCollection new.
  collec sortBlock: [ :a :b | a compare: b ].

  0 to: 257 do: [:x |
    |count|
    count := freq at: x.
    count > 0 ifTrue: [
      collec add: (Tree new: x count: count).
    ]
  ].

  collec size > 1 whileTrue: [
    |left right tree|
    left := collec last.
    collec removeLast.

    right := collec last.
    collec removeLast.

    tree := Tree new: left char count: (left count + right count).
    tree setLeft: left andRight: right.

    collec add: tree.
  ].

  tree := collec last.
  collec removeLast.

  encodingTable := getEncodingTable tree: tree.

  printTree ifTrue: [
    encodingTable do: [:node |
      |head|

      node char = 256 ifTrue: [ head := '%s'. ] ifFalse: [
        "printable range 33-126"
        node char >= 33 & node char <= 126 ifTrue: [
          head = ' %c '.
        ] ifFalse: [
          head = 'x%02X'.
        ].
      ].
      
      node char = 256 ifTrue: [
        writeStream nextPutAll: 'WILL BE REPLACED WITH THE RIGHT STUFF LATER'.
        "writeStream nextPutAll: head , '%8d %s\n' , "
      ] ifFalse: [
        writeStream nextPutAll: 'WILL BE REPLACED WITH THE RIGHT STUFF LATER'.
      ].
    ].
  ] ifFalse: [
    readStream reset.

    "not sure what to do with: if (readStream.atEnd()) return;"

    postOrderWrite tree: tree stream: writeStream.

    "wouldnt dare forget this 1!"
    writeStream writeBits: '1'.

    encodingMap = Array new: 257.
    encodingTable do: [:node |
      encodingMap at:(node char) put: (node bitpath).
    ].

    [ readStream atEnd ] whileFalse: [
      |bits|
      bits := encodingMap at:(readStream readByte).
      writeStream writeBits: bits.
    ].
    writeStream writeBits: (encodingMap at:256).
  ].
].

"
  uncompress file
"
uncompress := [ :readStream :writeStream |
  |stack isLeaf tree node|
  "not sure what to do with: if (readStream.atEnd()) return;"

  "dont know how big to make the array"
  stack := Collection new.

  [ readStream atEnd ] whileFalse: [
    |char|
    isLeaf := readStream readByte.

    isLeaf = 0 ifTrue: [
      char := readStream readByte.

      char = 0 ifTrue: [
        |extraBit|
        extraBit := readStream readBit.
        extraBit = 1 ifTrue: [ char := 256. ].
      ].

      stack addLast: (Tree new: char count: nil).
    ] ifFalse: [
      "dont know how to break"
      "stack length = 1 ifTrue: break."

      [ stack length > 1 ] whileTrue: [
        |left right branch|
        right := stack last.
        stack removeLast.

        left := stack last.
        stack removeLast.

        "skipping (!left || !right) check"

        branch = Tree new: nil count: nil.
        branch setLeft: left andRight: right.
        stack addLast: branch.
      ].
    ].
  ].

  tree := stack last.
  node := tree.

  [ readStream atEnd ] whileFalse: [
    |bit|
    bit := readStream readBit.

    bit = 0 ifTrue: [
      node := (node lnode).
    ] ifFalse: [
      node := (node rnode).
    ].

    node lnode isNil & node rnode isNil & node char ~= 256 ifTrue: [
      writeStream writeByte: node char.
      node = tree.
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
  stderr << 'Error: ' << err messageText << nl
    << 'Usage: -dtcu inputfile [outputfile]' << nl.
  ObjectMemory quit: 1.
].

" open read and write streams, and catch file errors "
[
  readStream := BitStream new: inputfile readNotWrite: true.
  writeStream := BitStream new: outputfile readNotWrite: false.
] on: SystemExceptions.FileError do: [:err |
  " Print file open error and exit "
  stderr << 'File Error: ' << err messageText << nl.
  ObjectMemory quit: 1.
].

" compress, decompress, or print tree depending on given args "
mode = $u ifTrue: [
  uncompress readStream: readStream writeStream: writeStream.
] ifFalse: [
  compress readStream: readStream 
      writeStream: writeStream printTree: (mode = $t).
].

" close file streams "
readStream close.
writeStream close.
