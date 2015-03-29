function shuffle(array) {
    for (var i = array.length - 1; i > 0; i--) {
        var randomIndex = Math.floor(Math.random() * (i + 1));
        swap(array, i, randomIndex);
    }
}

function swap(array, i1, i2) {
    var tmp = array[i1];
    array[i1] = array[i2];
    array[i2] = tmp;
}