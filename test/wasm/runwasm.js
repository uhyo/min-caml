#!/usr/bin/env node

if ('object' !== typeof WebAssembly){
    console.error('runwasm.js: WebAssembly object is undefined.');
    console.error('Possibly the version of node is too old (current version: %s', process.version);
    console.error('To run WebAssembly, node.js 8.0.0 or later is required.');
    process.exit(1);
}

const fs = require('fs');
const importObject = require('./libmincaml.js');

// Get target file name.
const wasm = process.argv[2];

// Load the wasm file.
const buf = fs.readFileSync(wasm);

// Instantiate a webassembly module.
WebAssembly.instantiate(buf, importObject)
.then(()=> process.exit(0))
.catch(err=>{
    console.error(err);
    process.exit(1);
});


