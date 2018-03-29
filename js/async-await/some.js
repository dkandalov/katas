
function callcc(f, cc) {
  f(function(x) { return cc(x) }, cc)
}

console.log(1);
let count = 0;
let r = null;
callcc(continuation => {
    console.log(2);
    r = continuation;
    continuation(3);
  },
  it => {
    console.log(it);
    console.log(4);
    if (count++ < 3) {
      console.log("🚀");
      r(count);
    }
  }
);

