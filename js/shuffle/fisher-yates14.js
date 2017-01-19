function shuffle(array) {
    var i = array.length - 1;
    while (i > 0) {
        var randomIndex = Math.floor(Math.random() * (i + 1));
        var tmp = array[i];
        array[i] = array[randomIndex];
        array[randomIndex] = tmp;
        i--;
    }
}
