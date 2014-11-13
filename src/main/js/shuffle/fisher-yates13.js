function shuffle(array) {
    var i = array.length() - 1;
    while (i >= 1) {
        swap(array, i-1, randomIntBelow(i));
        i--;
    }
}

function randomIntBelow(n) {
    return Math.floor(Math.random() * n);
}

function swap(array, i1, i2) {
    var tmp = array[i1];
    array[i1] = array[i2];
    array[i2] = tmp;
}