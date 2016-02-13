function f() { var x = 0; return function() { return ++x; }; }
var g = f();
g();
g();
