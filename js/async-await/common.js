module.exports = function() {
    expectToEqual = function(actual, expected)  {
      if (actual !== expected) {
        console.trace();
        throw `Expected: ${expected} but was ${actual}`;
      }
    };
}
