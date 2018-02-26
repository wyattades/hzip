#!/afs/cats.ucsc.edu/courses/cmps112-wm/usr/smalltalk/bin/gst -f
"#!/opt/smalltalk/bin/gst -f"

"

  Wyatt Ades and Lee White
  hzip.st

"


" Globals "
nl := Character nl.
Smalltalk at: #compareChar put: false.

Object subclass: BitStream [
  | filename readNotWrite fileStream bitindex byte |

  BitStream class >> new: filename readNotWrite: RNW [
    |result|
    result := self new.
    result init: filename readNotWrite: RNW.
    ^result.
  ]

  init: _filename readNotWrite: RNW [
    filename := _filename.  
    readNotWrite := RNW.
    self reset.
  ]

  reset [
    fileStream isNil ifTrue: [ self filestream close. ].

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
    chunk := self filestream next.

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
]

Object subclass: Tree [
  | char count bitpath lnode rnode |
  char [ ^ char ]
  count [ ^ count ]
  bitpath [ ^ bitpath ]
  lnode [ ^ lnode ]
  rnode [ ^ rnode ]

  Tree class >> new: _char count: _count [
    |result|
    result := self new.
    result init: _char count: _count.
    ^ result
  ]

  init: _char count: _count [
    char := _char.
    _count := _count.
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

getEncodingTable := [
  | collec _recurse |

  Smalltalk at: #compareChar put: true.

  collec := SortedCollection new.
  " need this????: collec sortBlock: [ :a :b | a compare: b ]. "

  _recurse := [ :node :bitpath |
    node lnode isNil ifFalse: [
      _recurse node: node lnode bitpath: bitpath , '0'.
    ].
    node rnode isNil ifFalse: [
      _recurse node: node rnode bitpath: bitpath , '1'.
    ].

    node lnode isNil & node rnode isNil ifTrue: [
      node setBitpath: bitpath.
      collec add: node.
    ].
  ].

  _recurse node: self bitpath: ''.

  ^collec
]

postOrderWrite := [ :stream |
  | _recurse |
  _recurse := [ :node |
    node lnode isNil ifFalse: [
      _recurse node: node lnode.
    ].
    node rnode isNil ifFalse: [
      _recurse node: node rnode.
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

  _recurse node: self.
]

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

  encodingTable := tree getEncodingTable.

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
].

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


[
  | mode inputfile outputfile |

" Parse arguments "

  Smalltalk arguments: '-d -c -t -u'
  do: [:opt :arg |
    arg = nil ifTrue: [
      (opt = $c | opt = $t | opt = $u) ifTrue: [
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

  [
    readStream := BitStream new: inputfile readNotWrite: true.
    writeStream := BitStream new: outputfile readNotWrite: false.

    mode = $u ifTrue: [
      uncompress readStream: readStream writeStream: writeStream.
    ] ifFalse: [
      compress readStream: readStream 
          writeStream: writeStream printTree: mode = $t.
    ].

    readStream close.
    writeStream close.
  ] on: SystemExceptions.FileError do: [:err |
    " Print file open error and exit "
    stderr << 'Error: ' << err messageText << nl.
    ObjectMemory quit: 1.
  ].

] on: Exception do: [ :err |
  " Print parsing error, print usage, and exit "
  stderr << 'Error: ' << err messageText << nl
    << 'Usage: -dtcu inputfile [outputfile]' << nl.
  ObjectMemory quit: 1.
].
