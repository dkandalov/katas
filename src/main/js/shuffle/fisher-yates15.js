function shuffle(array) {
    var i = array.length - 1;
    while (i >= 1) {
        var randomIndex = Math.floor(Math.random() * (i + 1));
        swap(i, randomIndex, array);
        i--;
    }
}

function swap(i1, i2, array) {
    var temp = array[i1];
    array[i1] = array[i2];
    array[i2] = temp;
}