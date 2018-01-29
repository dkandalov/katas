require("./common")();

let events = [];

function log(event) {
  events.push(event);
}

log("main 1");
let p = new Promise((resolve, reject) => {
  log("promise 2");
  resolve();
  log("promise 3");
}).then(() => {
  log("promise 4");
  throw new Error("Epic Fail");
  log("promise 5");
}).catch((e) => {
  log("promise 6 " + e);
}).then(() => {
  log("promise 7");
});
log("main 8");

// Expectation must be done in setTimeout() because promise callbacks will be executed
// after the completion of the current run of the JavaScript event loop
// (see https://developer.mozilla.org/en-US/docs/Web/JavaScript/EventLoop#Run-to-completion)
setTimeout(() => {
  expectToEqual(events, ["main 1",
    "promise 2",
    "promise 3",
    "main 8",
    "promise 4",
    "promise 6 Error: Epic Fail",
    "promise 7"
  ]);
}, 0);
