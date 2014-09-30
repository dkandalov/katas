function sort(array, index) {
    if (index == null) index = array.length - 1;
    if (index == 0) return;

    var randomIndex = Math.floor(Math.random() * (index + 1));
    swap(array, index, randomIndex);

    sort(array, index - 1);
}

function swap(array, i1, i2) {
    var temp = array[i1];
    array[i1] = array[i2];
    array[i2] = temp;
}