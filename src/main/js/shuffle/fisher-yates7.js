function shuffle(array) {
    var i = array.length;
    while (i > 0) {
        var j = Math.floor(Math.random() * i--);
        var tmp = array[i];
        array[i] = array[j];
        array[j] = tmp;
    }
}