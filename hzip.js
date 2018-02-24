const fs = require('fs');
const minimist = require('minimist');
const { sprintf } = require('sprintf-js');

// Global Variables:

let compareChar = false;

// Bit helper functions:

Number.prototype.setBit = function(index, bit) {
  if (index < 0 || index > 7) throw new Error('setBit out of bounds');

  const mask = 1 << index;
  if (bit === 0) return this & ~mask;
  else if (bit === 1) return this | mask;
  else throw new Error('setBit invalid bit');
};

Number.prototype.isBitSet = function(index) {
  if (index < 0 || index > 7) throw new Error('isBitSet out of bounds');

  const mask = 1 << index;
  return (this & mask) !== 0;
};

class BitStream {
  constructor(fileStream, readNotWrite) {
    this.fileStream = fileStream;
    this.bitindex = readNotWrite ? 0 : 7;
    this.byte = 0;

    // *** Only in js
    if (readNotWrite) {
      this.atEnd = false;
      this.fileStream.on('end', () => {
        this.atEnd = true;
      });
    }
  }

  readBit() {
    this.bitindex--;
    if (this.bitindex < 0) {
      this.bitindex = 7;
      this.byte = this.readByte();
    }
    return this.byte.isBitSet(this.bitindex) ? 1 : 0;
  }

  readByte() {
    return this.fileStream.read(1)[0];
  }

  writeBits(bits) { // this assumes `bits` is a string
    for (let i = 0; i < bits.length; i++) {
      let bit = bits.charCodeAt(i) - 48; // 0 or 1

      this.byte = this.byte.setBit(this.bitindex, bit);
      this.bitindex--;
      if (this.bitindex < 0) {
        this.writeByte(this.byte);
        this.byte = 0;
        this.bitindex = 7;
      }
    }
  }

  writeByte(byte) {
    // i.e. this.fileStream.putChar(byte)
    this.fileStream.write(Uint8Array.from([ byte ])); 
  }

  flush() {
    if (this.bitindex !== 7) this.writeByte(this.byte);
  }

  atEnd() {
    //                         i.e. this.fileStream.atEnd()
    return this.bitindex === 7 && this.atEnd; 
  }
}

class Tree {

  constructor(char, count) {
    this.char = char;
    this.count = count;
    this.lnode = null;
    this.rnode = null;
    this.bitpath = null;
  }

  setNodes(left, right) {
    this.lnode = left;
    this.rnode = right;
  }

  setBitpath(bitpath) {
    this.bitpath = bitpath;
  }

  compare(b) {
    if (compareChar || this.count === b.count) {
      return this.char > b.char;
    } else {
      return this.count > b.count;
    }
  }

  async getEncodingTable() {
    compareChar = true;
    
    const collec = new SortedCollection();

    const _recurse = async (node, bitpath) => {

      if (node.lnode !== null)
        await _recurse(node.lnode, bitpath + '0');
      if (node.rnode !== null)
        await _recurse(node.rnode, bitpath + '1');

      if (node.lnode === null && node.rnode === null) {
        node.setBitpath(bitpath);
        collec.add(node);
      }
    };

    await _recurse(this, '');

    return collec;
  }

  async postorder(stream) {

    const _recurse = async (node) => {

      if (node.lnode !== null)
        await _recurse(node.lnode);
      if (node.rnode !== null)
        await _recurse(node.rnode);

      if (node.lnode === null && node.rnode === null) {
        if (node.char === 0 || node.char === 256)
          stream.write('000000000' + (node.char === 0 ? '0' : '1'));
        else
          stream.write('0' + sprintf('%08b', node.char));
      } else {
        stream.write('1');
      }
    };

    await _recurse(this);
  }
}

// *** Only in js
class SortedCollection {

  constructor() {
    this.arr = [];
  }

  size() {
    return this.arr.length;
  }

  add(thing) {
    const len = this.arr.length;

    let i = 0;
    while (i < len && thing.compare(this.arr[i])) i++;

    this.arr.splice(i, 0, thing);
  }

  shift() {
    if (this.arr.length === 0) {
      return null;
    } else {
      return this.arr.shift();
    }
  }

  map(fn) {
    return this.arr.map(fn);
  }
}

const compress = async (data, printTree) => {

  const freq = new Array(257).fill(0);

  for (const char of data) {
    freq[char]++;
  }
  freq[256] = 1;

  const collec = new SortedCollection();
  
  for (let i = 0; i < freq.length; i++) {
    const count = freq[i];
    if (count > 0) {
      collec.add(new Tree(i, count));
    }
  }

  while (collec.size() > 1) {
    const left = collec.shift();
    const right = collec.shift();

    const tree = new Tree(left.char, left.count + right.count);
    tree.setNodes(left, right);

    collec.add(tree);
  }

  const tree = collec.shift();

  const encodingTable = await tree.getEncodingTable();

  if (printTree) {

    return encodingTable.map((node) => {
            
      let head;
      if (node.char === 256) head = '%s';
      // printable in range [33,126]
      else if (node.char >= 33 && node.char <= 126) head = ' %c ';
      else head = 'x%02X';

      return sprintf(head + '%8d  %s\n',
          node.char === 256 ? 'EOF' : node.char, node.count, node.bitpath);
    }).join('');

  } else if (data.length === 0) {
    return '';
  } else {

    let res = '';

    const stream = {
      write: (str) => {
        res += str;
      },
    };

    await tree.postorder(stream);

    // Convert SortedCollection to HashMap where <key>,<value>=char,bitpath
    const encodingMap = {};
    encodingTable.map((node) => {
      encodingMap[node.char] = node.bitpath;
    });

    // read file again
    for (const char of data) {
      stream.write(encodingMap[char]);
    }
    stream.write(encodingMap[256]);

    const padding = res.length % 8;
    if (padding > 0)
      res += '0'.repeat(8 - padding);

    // Convert string to Uint8Array
    return Uint8Array.from(res.match(/.{8}/g).map(el => {
      return Number.parseInt(el, 2);
    }));
  }
};

const uncompress = async (data) => {

  if (data.length === 0) return '';

  // Convert bytes to string of bits
  const str = data.map(el => sprintf('%08b', el)).join('');

  let i = 0;
  const stack = [];

  while (true) {
    if (i >= str.length) {
      console.error('Stack never emptied');
      process.exit(1);
    }

    const isLeaf = str.charAt(i++);

    if (isLeaf === '0') {
      let char = Number.parseInt(str.substring(i, i + 8), 2);
      i += 8;

      if (char === 0 && str.charAt(i++) === '1') { // char = EOF
        char = 256;
      }

      stack.push(new Tree(char));
    } else {

      if (stack.length === 1) break;

      const right = stack.pop();
      const left = stack.pop();
      if (!left || !right) console.error('REAL BAD');
      const branch = new Tree();
      branch.setNodes(left, right);
      stack.push(branch);

    }
  }

  i--; // Sometimes makes it work

  const tree = stack.pop();

  let node = tree;
  const res = [];

  for ( ; true; i++) {

    if (i >= str.length) {
      console.error('Never found EOF');
      process.exit(1);
    }

    const bit = str.charAt(i);
    if (!bit) console.error('UH OH');

    if (bit === '0') node = node.lnode;
    else node = node.rnode;

    if (node.lnode === null && node.rnode === null) {  
      if (node.char === 256) break;

      res.push(node.char); // i.e. stream.write
      node = tree;
    }
  }

  return Uint8Array.from(res);
};

const main = () => {
  const argv = minimist(process.argv, {
    boolean: ['c','u','d','t'],
  });
  
  const inputfile = argv._[2];
  const outputfile = argv._[3];
  
  if (argv._.length > 4 ||
      !inputfile ||
      [argv.c, argv.u, argv.t].reduce((c, a) => a ? c + 1 : c, 0) !== 1) {
  
    console.error('Usage: -cudt inputfile [outputfile]')
    process.exit(1);
  }
  
  // Converted to array
  const data = [...fs.readFileSync(inputfile)];
  
  // const readStream = new BitStream();
  
  (argv.u ? uncompress(data) : compress(data, argv.t))
  .then((result) => {
    if (outputfile) fs.writeFileSync(outputfile, result);
    else process.stdout.write(result);
  })
  .catch(console.error);
};

main();

