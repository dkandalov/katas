require("./common")();

let events = [];

function log(event) {
  events.push(event);
}

// Send/receive values from coroutine

function* cc(n) {
  log(`c received: ${n}`);
  n = yield 2;
  log(`c received: ${n}`)
}

log("main started");
let c = cc();
let n = c.next(1);
log("main received: " + JSON.stringify(n));
n = c.next(3);
log("main received: " + JSON.stringify(n));
log("main finished");

expectToEqual(events, [
  'main started',
  'c received: undefined',
  'main received: {"value":2,"done":false}',
  'c received: 3',
  'main received: {"done":true}',
  'main finished'
]);
