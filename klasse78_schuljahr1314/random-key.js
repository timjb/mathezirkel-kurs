#!/usr/bin/env node

var alphabet = '';
for (var i = 0; i < 26; i++) {
  alphabet += String.fromCharCode('a'.charCodeAt(0) + i);
}

var schluessel = '';
while (alphabet.length > 0) {
  var j = Math.floor(Math.random() * alphabet.length);
  schluessel += alphabet.charAt(j);
  alphabet = alphabet.slice(0,j) + alphabet.slice(j+1);
}

console.log(schluessel);
