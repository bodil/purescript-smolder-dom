exports.runWithDom = function(eff) {
  return function() {
    var JSDOM = require("jsdom").JSDOM;
    var old = global.window;
    global.window = new JSDOM().window;
    var result = eff();
    global.window = old;
    return result;
  };
};

exports.innerHTML = function(node) {
  return node.innerHTML;
};
