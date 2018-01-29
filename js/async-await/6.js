require("./common")();

let events = [];

function log(event) {
  events.push(event);
}

function f() {
  return 42;
}

async function a() {
  log("a 2");
  await f();
  log("a 3");
  await f();
  log("a 4");
}

async function b() {
  log("b 2");
  await f();
  log("b 3");
  await f();
  log("b 4");
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
    'a 2',
    'b 2',
    'main [object Promise]',
    'main [object Promise]',
    'main end',
    'a 3',
    'b 3',
    'a 4',
    'b 4'
  ]);
}, 0);
