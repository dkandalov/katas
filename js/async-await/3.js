require("./common")();

let events = [];
function log(event) {
  events.push(event);
}

// Yield/resume from generator subfunction

function* subfunction() {
  log("subfunction started");
  console.trace();
  yield 42;
  log("subfunction finished");
}

function* cc() {
  log("coroutine started");
  let sf = subfunction();
  sf.next();
  sf.next();
  log("coroutine finished");
}

let c = cc();
var n = c.next().value;
console.log(n);
n = c.next().value;
console.log(n);
n = c.next().value;
console.log(n);

console.log(events);
