function shuffle(array) {
    var n = array.length;
    while (n > 0) {
        var randomIndex = Math.floor(Math.random() * n);
        swapIn(array, n - 1, randomIndex);
        n--;
    }
}

function swapIn(array, i1, i2) {
    var temp = array[i1];
    array[i1] = array[i2];
    array[i2] = temp;
}