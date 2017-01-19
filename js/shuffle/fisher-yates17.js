function shuffle(array) {
    var i = array.length;
    while (i > 0) {
        var randomIndex = Math.floor(Math.random() * i);
        swap(array, --i, randomIndex);
    }
}

function swap(array, i1, i2) {
    var tmp = array[i1];
    array[i1] = array[i2];
    array[i2] = tmp;
}
