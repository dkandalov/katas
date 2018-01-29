require("./common")();

let events = [];

function log(event) {
  events.push(event);
}

// Yield/resume from generator subfunction

function* subfunction() {
  let f = () => {
    // yield 42; // fails to run even though the outer function is generator
  }
  log("subfunction started");
  // log(stackTrace());
  yield 42; // can't yield without marking function as generator
  log("subfunction finished");
}

function* cc() {
  log("coroutine started");
  let sf = subfunction();
  yield sf.next().value;
  yield sf.next().value;
  log("coroutine finished");
}

let c = cc();
log("received: " + JSON.stringify(c.next()));
log("received: " + JSON.stringify(c.next()));
log("received: " + JSON.stringify(c.next()));

expectToEqual(events, [
  "coroutine started",
  "subfunction started",
  "received: {"value":42,"done":false}",
  "subfunction finished",
  "received: {"done":false}",
  "coroutine finished",
  "received: {"done":true}"
]);
