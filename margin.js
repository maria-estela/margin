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
    json.push([Date(), s]);
    write(json);
}
var a = process.argv;
var n = a.shift();
if(n==='node'){ a.shift(); }
add(a.join());
