function shuffle(array) {
    var i = 0;
    var n = array.length;
    while (n > 0) {
        var newIndex = Math.floor(Math.random() * n) + i;
        swap(array, i, newIndex);
        i++;
        n--;
    }
}

function swap(array, i, newIndex) {
    var temp = array[i];
    array[i] = array[newIndex];
    array[newIndex] = temp;
}