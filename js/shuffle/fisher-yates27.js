function shuffle(array) {
    var i = array.length;
    while (i > 1) {
        const randomIndex = Math.floor(Math.random() * i);
        swap(array, i - 1, randomIndex);
        i--;
    }
}

function swap(array, i1, i2) {
    const tmp = array[i1];
    array[i1] = array[i2];
    array[i2] = tmp;
}
