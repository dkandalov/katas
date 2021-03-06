require("./common")();

let events = [];

function log(event) {
  events.push(event);
}

// Yield from coroutine

function* cc() {
  log("c started");
  yield;
  log("c finished");
}

log("main started");
let c = cc();
c.next();
c.next();
c.next();
log("main finished");

expectToEqual(events, [
  'main started',
  'c started',
  'c finished',
  'main finished'
]);
