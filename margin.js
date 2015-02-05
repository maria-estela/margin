/*
    > JSON.stringify(z(['node', 'margin', '50', 'random', 'words']))
    '{"n":50,"d":"random words"}'
*/
function z(a) {
    if(a[0]==='node'){
        a.shift();
    }
    a.shift();
    var r={n:Number(a.shift())};
    r.d=a.join(' ');
    return r;
};
var fs = require('fs');
function write(json) {
    var s = JSON.stringify(json);
    var b = fs.openSync('file.json', 'w+');
    fs.writeSync(b, s);
}
function read() {
    var s = fs.readFileSync('file.json')
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
a.t=Date();
add(a);
