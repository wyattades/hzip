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
  |filename readNotWrite fileStream bitindex byte|

  filename [ ^ filename ]
  readNotWrite [ ^ readNotWrite ]
  fileStream [ ^ fileStream ]
  bitindex [ ^ bitindex ]
  byte [ ^ byte ]

  BitStream class >> new: filename readNotWrite: RNW [
    |result|
    result := self new.

    result filename: filename.
    result readNotWrite: RNW.
    ^result.
  ]

  setBitIndex: index [
    bitindex := index.
  ]

  setByte: abyte [
    byte := abyte.
  ]

  setFileStream: file createNotRead: CNR [
    file = 'stdout' isTrue: [ filename := stdout. ] isFalse: [
      CNR isTrue: [
        fileStream := FileStream open: file mode: FileStream create.
      ] isFalse: [
        fileStream := FileStream open: file mode: FileStream read.
      ].
    ]
  ]

  init [
    fileStream isNil isFalse: [
      self setBitIndex: 7.
      self setByte: 0.

      readNotWrite isTrue: [
        self setFileStream: filename createNotRead: false.
      ] isFalse: [
        filename isNil isTrue: [
          self setFileStream: 'stdout' createNotRead: false.
        ] isFalse: [].
          self setFileStream: filename createNotRead: true.
      ].
    ] isTrue: [
      self filestream close.
    ].
  ]

  readBit [
    |bit|
    bitindex = 7 isTrue: [ byte := self readByte. ].
    "dont know what byte.isbitset is"
    "confirmed"
    bit := 0.
    bitindex := bitindex - 1.
    bitindex < 0 isTrue: [ bitindex = 7. ].
    ^bit
  ]

  readByte [
    ^0000
"    |chunk newbyte res|
    chunk := self filestream next.

    bitindex = 7 isTrue: [
      ^newbyte
    ] isFalse: [
      ??
      res = ((this.newbyte << (7 - this.bitindex)) & 0xFF) |
          (newbyte >> (this.bitindex + 1)).

      byte = newbyte.
      ^res"
    "]."
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
    result := self new.
    result char: aChar.
    result count: aCount.
    ^result
  ]

  setLeft: aLeft andRight: aRight [
    lnode := aLeft.
    rnode := aRight.
  ]

  setBitpath: aPath [
    bitpath := aPath.
  ]

  compare: b [
    (compareChar) | (count = b count) isTrue: [
      ^ (char > b char)
    ] isFalse: [
      ^ (count > b count)
    ].
  ]

  getEncodingTable [
    |collec getETrecur|
    Smalltalk at: #compareChar put: true.

    collec := SortedCollection new.
    collec sortBlock: [ :a :b | a compare: b ].

    getETrecur := [ :node :bitpath |
      node lnode isNil isFalse: [
        getETrecur node: node lnode bitpath: bitpath , '0'.
      ].

      node rnode isNil isFalse: [
        getETrecur node: node rnode bitpath: bitpath , '1'.
      ].

      node lnode isNil & node rnode isNil isTrue: [
        node setBitpath: bitpath.
        collec add: node.
      ].
    ].
    getETrecur node: self bitpath: ''.

    ^collec
  ]

  postOrderWrite: stream [
    POWrecur := [ :node |
      node lnode isNil isFalse: [
        POWrecur node: node lnode.
      ].

      node rnode isNil isFalse: [
        POWrecur node: node rnode.
      ].

      node lnode isNil & node rnode isNil isTrue: [
        node char = 0 || node char = 256 isTrue: [
          stream nextPutAll: '000000000'.
          node char = 0 isTrue: [
            stream nextPut: '0'.
          ] isFalse: [
            stream nextPut: '1'.
          ].
        ] isFalse: [
          stream nextPut: '0'.
          node char printOn: stream base: 2.
        ]
      ] isFalse: [
        stream nextPut: '1'.
      ].
    ].

    POWrecur node: self.
  ]

]

Object subclass: Zipper [
  |freq collec|

  compress: readStream write: writeStream tree: printTree [
    |byte encodingTable encodingMap tree|
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
      count > 0 isTrue: [
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

    encodingTable := tree getEncodingTable.

    printTree isTrue: [
      encodingTable do: [:node |
        |head|

        node char = 256 isTrue: [ head := '%s'. ] isFalse: [
          "printable range 33-126"
          node char >= 33 & node char <= 126 isTrue: [
            head = ' %c '.
          ] isFalse: [
            head = 'x%02X'.
          ].
        ].
        
        node char = 256 isTrue: [
          writeStream nextPutAll: 'WILL BE REPLACED WITH THE RIGHT STUFF LATER'.
          "writeStream nextPutAll: head , '%8d %s\n' , "
        ] isFalse: [
          writeStream nextPutAll: 'WILL BE REPLACED WITH THE RIGHT STUFF LATER'.
        ].
      ].
    ] isFalse: [
      readStream init.

      "not sure what to do with: if (readStream.atEnd()) return;"

      tree postOrderWrite: writeStream.

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
  ]

  uncompress: readStream write: writeStream [
    |stack y isLeaf tree node|
    "not sure what to do with: if (readStream.atEnd()) return;"

    "dont know how big to make the array"
    stack := Collection new.

    y := 0.

    [ readStream atEnd ] whileFalse: [
      |char|
      isLeaf := readStream readByte.

      isLeaf = 0 isTrue: [
        char := readStream readByte.

        char = 0 isTrue: [
          |extraBit|
          extraBit := readStream readBit.
          extraBit = 1 isTrue: [ char := 256. ].
        ].

        stack addLast: (Tree new: char count: nil).
      ] isFalse: [
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

      bit = 0 isTrue: [
        node := (node lnode).
      ] isFalse: [
        node := (node rnode).
      ].

      node lnode isNil & node rnode isNil & node char ~= 256 isTrue: [
        writeStream writeByte: node char.
        node = tree.
      ].
    ].
  ]
]


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
    "zip compress: inputfile write: outputfile tree: false."
  ] ifFalse: [
    uncompress ifTrue: [
      "zip uncompress: inputfile write: outputfile."
    ] ifFalse: [
      tree ifTrue: [
        "zip compress: inputfile write: outputfile tree: true."
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
