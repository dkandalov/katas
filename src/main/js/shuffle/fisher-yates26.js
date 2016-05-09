function shuffle(array) {
    var i = array.length;
    while (i > 0) {
        var randomI = Math.floor(Math.random() * i);
        swap(randomI, i - 1, array);
        i--;
    }
}

function swap(i1, i2, array) {
    var temp = array[i1];
    array[i1] = array[i2];
    array[i2] = temp;
}
