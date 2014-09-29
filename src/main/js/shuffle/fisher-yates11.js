function sort(array) {
    for (var i = 0; i < array.length - 1; i++) {
        swap(array, i, randomIntBetween(i, array.length));
    }
}

function randomIntBetween(from, to) {
    return from + Math.floor(Math.random() * (to - from));
}

function swap(array, i1, i2) {
    var tmp = array[i1];
    array[i1] = array[i2];
    array[i2] = tmp;
}