#!/usr/bin/node

const fs = require('fs');
const minimist = require('minimist')
const sprintf = require('sprintf-js');

class Tree {

  constructor(char, count) {
    this.char = char;
    this.count = count;
    this.lnode = null;
    this.rnode = null;
    this.bitpath = null;
  }

  compare(b) {
    if (this.bitpath !== null || this.count === b.count) {
      return this.char > b.char;
    } else if (this.count > b.count) {
      return true;
    } else  {
      return false;
    }
  }

  recurse(size) {
    return new Promise(resolve => {
      const collec = new SortedCollection();

      const _recurse = (node, bitpath) => {

        if (node.lnode !== null) {
          _recurse(node.lnode, bitpath + '0');
        }
        if (node.rnode !== null) {
          _recurse(node.rnode, bitpath + '1');
        }

        if (node.lnode === null && node.rnode === null) {
          node.bitpath = bitpath;
          collec.add(node);
        }

        if (collec.size() === size) {
          resolve(collec);
        }
      };

      if (size === 0) resolve(collec);
      else _recurse(this, '');
    });
  }

  postorder(stream, size) {
    return new Promise(resolve => {
      let count = 0;

      const _recurse = (node) => {

        if (node.lnode !== null) {
          _recurse(node.lnode);
        }
        if (node.rnode !== null) {
          _recurse(node.rnode);
        }

        if (node.lnode === null && node.rnode === null) {
          if (node.char > 0 && node.char < 255)
            stream.write('0' + sprintf('%08b', node.char).split('').reverse().join(''));
          else stream.write('0' + '00000000' + (node.char === 0 ? '0' : '1'));
        } else {
          stream.write('1');
        }

        if (++count === size) {
          resolve();
        }
      };

      if (size === 0) resolve(collec);
      else _recurse(this);
    });
  }
}

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

  get(i) { // super inefficient to call on every character
    return this.arr.find(el => el.char === i);
  }

  map(fn) {
    return this.arr.map(fn);
  }
}

const compress = async (data, printTree) => {

  const freq = [];
  for (let i = 0; i < 256; i++) freq[i] = 0;

  let amount = 0;

  // not necessary
  const chars = [];

  for (let i = 0; i < data.length; i++) {
    const char = data.charCodeAt(i);
    chars.push(char);//temp
    freq[char]++;
  }
  freq[255] = 1;
  chars.push(255);//temp

  const collec = new SortedCollection();
  
  for (let i = 0; i < freq.length; i++) {
    const count = freq[i];
    if (count > 0) {
      collec.add(new Tree(i, freq[i]));
      amount++;
    }
  }

  const leafAmount = collec.size();

  while(collec.size() > 1) {
    const leastest = collec.shift();
    const least = collec.shift();

    const tree = new Tree(leastest.char, leastest.count + least.count);
    tree.lnode = leastest;
    tree.rnode = least;

    collec.add(tree);
  }

  const tree = collec.shift();

  // dont need `size` if its syncronous
  const encodingTable = await tree.recurse(leafAmount);

  if (printTree) {

    return encodingTable.map((node) => {
      
      const { char, count, bitpath } = node;
      
      let charString;
      if (char >= 32 && char <= 126)
        charString = ' ' + String.fromCharCode(char) + ' ';
      else if (char === 255)
        charString = 'EOF';
      else
        charString = 'x' + char.toString(16).toUpperCase();

      //  "%3d %5d %s" or "%3c %5d %s"
      return sprintf('%3c %5d %s', charString, count, bitpath);
      // return `${charString} ${count} ${bitpath}`;
    }).join('\n');

  } else {
    let res = '';
    let count = 0;

    const stream = {
      write: (str) => {
        count += str.length;
        res += str;// + '\n';
      },
    };

    await tree.postorder(stream, leafAmount * 2 - 1);

    // res += '----------------\n';

    for (let char of chars) {
      stream.write(encodingTable.get(char).bitpath);
    }

    res += '0'.repeat(8 - (count % 8));

    return Uint8Array.from(res.match(/.{8}/g).map(el => {
      return Number.parseInt(el);
    }));
    
    // return res;
  }
};

const uncompress = async (data) => {
  return '';
};

const argv = minimist(process.argv, {
  boolean: ['c','u','d','t'],
});
const inputfile = argv._[0];
const outputfile = argv._[1];

const data = fs.readFileSync(inputfile, { encoding: 'ascii' });

(argv.c ? compress(data, argv.t) : uncompress(data))
.then((result) => {
  if (outputfile) fs.writeFileSync(outputfile, result);
  else fs.writeFileSync(process.stdout, result);
})
.catch(console.error);

