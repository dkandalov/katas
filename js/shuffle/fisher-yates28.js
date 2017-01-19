function shuffle(array) {
    var i = array.length;
    while (i > 0) {
        const randomIndex = Math.floor(Math.random() * i);
        swap(i - 1, randomIndex, array);
        i--;
    }
}

function swap(i1, i2, array) {
    const tmp = array[i1];
    array[i1] = array[i2];
    array[i2] = tmp;
}