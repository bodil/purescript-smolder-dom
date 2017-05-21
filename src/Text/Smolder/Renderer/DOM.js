exports.makeElement = function(name) {
  return function() {
    return window.document.createElement(name);
  };
};

exports.makeText = function(text) {
  return function() {
    return window.document.createTextNode(text);
  };
};
