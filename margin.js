#!/usr/bin/env node
'use strict';

/*
    > JSON.stringify(z(['node', 'margin', '50', 'random', 'words']))
    '{"n":50,"d":"random words"}'
*/
function z(a) {
    if(a[0]==='node'){
        a.shift();
    }
    a.shift();
    var r={value:Number(a.shift())};
    r.description=a.join(' ');
    return r;
};
var fs = require('fs'),
    pd = require('pretty-data').pd,
    fileName = 'margin-data.json';
function write(json) {
    var s = pd.json(json);
    var b = fs.openSync(fileName, 'w+');
    fs.writeSync(b, s);
}
function read() {
    var s = fs.readFileSync(fileName)
    return JSON.parse(s.toString())
}
function add(s) {
    var json;
    try{
        json = read();
    } catch(e) {
        json = [];
    }
    json.push(s);
    write(json);
}
var a = z(process.argv);
var bar = '';
for (var i=0; i<a.n; i++) {
    bar += '.';
}
console.log(bar);
a.time = new Date().toJSON();
add(a);
