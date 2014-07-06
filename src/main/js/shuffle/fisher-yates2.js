function shuffle(array) {
    var n = array.length;
    while (n > 0) {
        var newIndex = Math.floor(Math.random() * n);
        var temp = array[n-1];
        array[n-1] = array[newIndex];
        array[newIndex] = temp;
        n--;
    }
}