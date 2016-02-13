function f() { var x = 0; return function() { return ++x; }; }
var g = f();
var h = f();
g();
g();
h();
