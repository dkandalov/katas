module.exports = function() {
  expectToEqual = function(actual, expected) {
    if (JSON.stringify(actual) != JSON.stringify(expected)) {
      console.log("Expected")
      console.log(expected);
      console.log("but was:");
      console.log(actual);
      console.trace();
      throw "Assertion failed";
    }
  };
  stackTrace = function() {
    var err = new Error();
    return err.stack;
  };
}
