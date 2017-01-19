// see also https://devnet.jetbrains.com/thread/459541

function hello1() {
    console.log("hello1");
}
var lib2 = function(context) {
    context.hello2 = function() {
        console.log("hello2");
    };
};
var lib3 = function(context) {
    /** @name hello3 */
    context.hello3 = function() {
        console.log("hello3");
    };
};

function main() {
    var importedLib = {};
    lib2(importedLib);
    lib3(this);

    hello1();             // resolved
    importedLib.hello2(); // resolved
    hello3();             // not resolved
}
