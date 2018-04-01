# hzip
Compress and uncompress files with Huffman encoding.

Written in both JavaScript (`hzip.js`) and Smalltalk (`hzip.st`).

## Usage

- Smalltalk:  
`gst -f hzip.st -tcu inputfile [outputfile]`  
(gst = the GNU Smalltalk virtual machine)

- JavaScript:  
`npm install`  
`node hzip.js -tcu inputfile [outputfile]`

**inputfile** file to be read for compression/uncompression  
**outputfile** compressed/uncompressed file (otherwise stdout) 
 
Provide exactly one of:  
**-t** output pretty-printed huffman encoding tree  
**-c** compress file  
**-u** uncompress file  
