#!/opt/smalltalk/bin/gst -f
" #!/afs/cats.ucsc.edu/courses/cmps112-wm/usr/smalltalk/bin/gst -f "

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


Object subclass: Zipper [
  |stream freqTable|
  Zipper class >> new: _stream [
    |result|
    result := super new.
    result init: _stream.
    ^result
  ]
  init: fileStream [
    stream := _stream.
  ]
  compress: _stream andTree: isTree [

  ]
  uncompress: _stream [
    
  ]
].

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
