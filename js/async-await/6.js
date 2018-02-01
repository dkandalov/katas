require("./common")();

let events = [];

function log(event) {
  events.push(event);
}

function f() {
  return 42;
}

async function a() {
  log("a 1");
  await f();
  log("a 2");
  await f();
  log("a 3");
}

async function b() {
  log("b 1");
  await f();
  log("b 2");
  await f();
  log("b 3");
}

log("main start");
let result1 = a();
let result2 = b();
log("main " + result1);
log("main " + result2);
log("main end");

setTimeout(() => {
  expectToEqual(events, [
    'main start',
    'a 1',
    'b 1',
    'main [object Promise]',
    'main [object Promise]',
    'main end',
    'a 2',
    'b 2',
    'a 3',
    'b 3'
  ]);
}, 0);
