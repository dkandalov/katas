let events = [];
function log(event) {
  events.push(event);
}

// Send/receive values from coroutine

function* cc(n) {
  log(`c received: ${n}`)
  n = yield 2;
  log(`c received: ${n}`)
}

log("main started");
let c = cc();
var n = c.next(1).value;
log(`main received: ${n}`)
n = c.next(3).value;
log(`main received: ${n}`)
log("main finished");

console.log(events);
