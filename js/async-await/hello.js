// From https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Using_promises
// ----------
// Then-catch
// ----------
console.log("before 1");
new Promise((resolve, reject) => {
    console.log("Initial");
    resolve();
  }).then(() => {
    throw new Error("Something failed");
    console.log("Do this");
  }).catch((e) => {
    console.log("Do that");
  }).then(() => {
    console.log("Do this whatever happened before");
  });
console.log("after 1");
console.log("after 1");
// Output:
// Initial, Do that, Do this whatever happened before

let output = [];
new Promise(function(resolve, reject) {
  setTimeout(() => resolve(1), 2000);
}).then((result) => {
  console.log(result);
  return result + 2;
}).then((result) => {
  console.log(result);
  return result + 2;
}).then((result) => {
  console.log(result);
  return result + 2;
});
// Output:
// 1, 3, 5


// ------
// Timing
// ------
const wait = ms => new Promise(resolve => setTimeout(resolve, ms));
wait(0).then(() => console.log(4));
Promise.resolve()
  .then(() => console.log(2))
  .then(() => console.log(3));
console.log(1);
// Output:
// 1, 2, 3, 4
