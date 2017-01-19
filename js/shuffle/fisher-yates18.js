function shuffle(array) {
    var i = array.length - 1;
    while (i > 0) {
        var randomIndex = Math.floor(Math.random() * (i + 1));
        swap(array, i, randomIndex);
        i--;
    }
}

function swap(array, i1, i2) {
    var tmp = array[i1];
    array[i1] = array[i2];
    array[i2] = tmp;
}
