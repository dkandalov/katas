// see http://bost.ocks.org/mike/shuffle/compare.html
function shuffle(array) {
    var i = 0;
    while (i < array.length) {
        var randomI = i + Math.floor(Math.random() * (array.length - i));
        swap(i, randomI, array);
        i++;
    }
}

function swap(i1, i2, array) {
    var tmp = array[i1];
    array[i1] = array[i2];
    array[i2] = tmp;
}