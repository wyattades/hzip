const fs = require('fs');
const minimist = require('minimist');
const { sprintf } = require('sprintf-js');

// Global Variables:

let compareChar = false;
let x = 0;
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
  constructor(filename, readNotWrite) {
    this.filename = filename;
    this.readNotWrite = readNotWrite;
    this.fileStream = null;
  }

  init() {
    return new Promise((resolve) => {
      if (this.fileStream !== null) this.close();

      this.bitindex = 7;
      this.byte = 0;

      if (this.readNotWrite) {
        this.fileStream = fs.createReadStream(this.filename, {
          autoClose: false,
          end: fs.statSync(this.filename).size,
        });
        this.fileStream.pause();

        // *** Only in js
        this.fileIndex = 0;
        this.fileStream.on('readable', resolve);
        this.fileStream.on('error', console.error);

      } else {
        this.fileStream = this.filename !== null ?
          fs.createWriteStream(this.filename, { autoClose: false }) : process.stdout;

        resolve();
      }
    });
  }

  readBit() {
    if (this.bitindex === 7) {
      this.byte = this.readByte();
    }
    const bit = this.byte.isBitSet(this.bitindex) ? 1 : 0;
    this.bitindex--;
    if (this.bitindex < 0) {
      this.bitindex = 7;
    }
    return bit;
  }

  // This is the function that's broke
  readByte() {
    const chunk = this.fileStream.read(1); // i.e. this.fileStream.next()
    this.fileIndex++; // *** only js
    // if (this.uncompress) console.log('b:', chunk[0]);
    let byte;

    // *** only js
    if (chunk === null) {
      console.error('Error: No more bytes to read');
      byte = 0;
      process.exit(1);
    } else {
      byte = chunk[0];
    }

    if (this.bitindex === 7) {
      return byte;
    } else {
      // console.error('NOT ALLOWED SRY');
      const res = ((this.byte << (7 - this.bitindex)) & 0xFF) |
          (byte >> (this.bitindex + 1));
      // console.log(sprintf('f: %016b + %016b', ((this.byte << (7 - this.bitindex)) && 0xFF), (byte >> (this.bitindex + 1))));
      this.byte = byte;
      return res;
    }
  }

  async writeBits(bits) { // this assumes `bits` is a string
    for (let i = 0; i < bits.length; i++) {
      const bit = bits.charCodeAt(i) - 48; // 0 or 1

      this.byte = this.byte.setBit(this.bitindex, bit);
      this.bitindex--;
      if (this.bitindex < 0) {
        await this.writeByte(this.byte);
        this.byte = 0;
        this.bitindex = 7;
      }
    }

    // if (x++ < 20) 
    // console.log(bits);
  }

  writeByte(byte) {
    // i.e. this.fileStream.putChar(byte)
    return new Promise(resolve => 
      this.fileStream.write(Uint8Array.from([ byte ]), null, resolve)); 
  }

  writeString(string) {
    // i.e. self fileStream next: string length putAll: string startingAt: 0
    return new Promise(resolve => 
      this.fileStream.write(string, null, resolve));
  }

  async flush() {
    if (this.bitindex !== 7) await this.writeByte(this.byte);
  }

  atEnd() {
    //                         i.e. this.fileStream.atEnd()
    return this.bitindex === 7 && this.fileIndex >= this.fileStream.end; 
  }

  close() {
    if (this.filename !== null)
      this.fileStream.close();
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

  async forEach(fn) {
    for (const el of this.arr) {
      await fn(el);
    }
  }
}

const getEncodingTable = async (tree) => {
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

  await _recurse(tree, '');

  return collec;
}

const postorderWrite = async (tree, stream) => {

  const _recurse = async (node) => {
    if (node !== null) {
      await _recurse(node.lnode);
      await _recurse(node.rnode);

      if (node.lnode === null && node.rnode === null) {
        if (node.char === 0 || node.char === 256)
          await stream.writeBits('000000000' + (node.char === 0 ? '0' : '1'));
        else
          await stream.writeBits('0' + sprintf('%08b', node.char));
      } else {
        await stream.writeBits('1');
      }
    }
  };

  await _recurse(tree);
}

const compress = async (readStream, writeStream, printTree) => {

  const freq = new Array(257).fill(0);

  while (!readStream.atEnd()) {
    const byte = readStream.readByte();
    freq[byte]++;
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

  const encodingTable = await getEncodingTable(tree);

  if (printTree) {

    await encodingTable.forEach(async (node) => {
            
      let head;
      if (node.char === 256) head = '%s';
      // printable in range [33,126]
      else if (node.char >= 33 && node.char <= 126) head = ' %c ';
      else head = 'x%02X';

      await writeStream.writeString(sprintf(head + '%8d  %s\n',
          node.char === 256 ? 'EOF' : node.char, node.count, node.bitpath));
    });

  } else {
    await readStream.init();

    if (readStream.atEnd()) return;

    await postorderWrite(tree, writeStream);

    // Convert SortedCollection to HashMap where <key>,<value>=char,bitpath
    const encodingMap = {};
    await encodingTable.forEach(async (node) => {
      encodingMap[node.char] = node.bitpath;
    });

    // read inputfile again
    while (!readStream.atEnd()) {
      const bits = encodingMap[readStream.readByte()];
      await writeStream.writeBits(bits);
    }
    await writeStream.writeBits(encodingMap[256]);
  }
};


const uncompress = async (readStream, writeStream) => {

  // If inputfile is empty, we do nothing
  if (readStream.atEnd()) return;

  const stack = [];
  // readStream.uncompress = true;
  let y = 0;
  while (true) {
    if (readStream.atEnd()) {
      console.error('Stack never emptied');
      process.exit(1);
    }

    const isLeaf = readStream.readBit();
    // if (y < 20) {
    //   if (isLeaf === 0) process.stdout.write('0');
    //   else {y++; console.log('1'); };
    // }

    if (isLeaf === 0) {
      let char = readStream.readByte();
      
      // if (y < 20)process.stdout.write(sprintf('%08b', char));
      if (char === 0) {
        const extraBit = readStream.readBit();
        // if (y < 20)process.stdout.write(extraBit.toString());
        if (extraBit === 1) // char = EOF
          char += 256;
      }

      // if (y++ < 20) console.log('');

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

  const tree = stack.pop();
  let node = tree;

  while (true) {
    if (readStream.atEnd()) {
      console.error('Never found EOF');
      process.exit(1);
    }

    const bit = readStream.readBit();
    // console.log(bit);

    if (bit === 0) node = node.lnode;
    else node = node.rnode;

    if (node.lnode === null && node.rnode === null) {  
      if (node.char === 256) break;
      console.log('char', node.char);
      await writeStream.writeByte(node.char);
      node = tree;
    }
  }
};

const main = async (argv) => {
  
  // Check validity of args
  const inputfile = argv._[2];
  const outputfile = argv._[3] || null;
  
  if (argv._.length > 4 ||
      !inputfile ||
      [argv.c, argv.u, argv.t].reduce((c, a) => a ? c + 1 : c, 0) !== 1) {
  
    throw new Error('Usage: -cudt inputfile [outputfile]');
  }

  // Create read and write streams
  
  const readStream = new BitStream(inputfile, true);
  await readStream.init();
  const writeStream = new BitStream(outputfile, false);
  await writeStream.init();

  // Compress or uncompress

  if (argv.u) {
    console.log('Starting decompression...');
    await uncompress(readStream, writeStream);
    console.log('Finished decompression.');
  } else {
    console.log('Starting compression...');
    await compress(readStream, writeStream, argv.t);
    console.log('Finished compression.');
  }

  // Flush and close IO streams
  
  readStream.close();
  writeStream.flush();
  writeStream.close();
};


main(minimist(process.argv, {
  boolean: ['c','u','d','t'],
}))
.catch(e => {
  console.error(e);
  process.exit(1);
});
