// kind of Fisher-Yates shuffling as described here http://bost.ocks.org/mike/shuffle/compare.html
function shuffle(array) {
    var i = 0;
    while (i < array.length) {
        var randomIndex = i + Math.floor(Math.random() * (array.length - i));
        var temp = array[i];
        array[i] = array[randomIndex];
        array[randomIndex] = temp;
        i++;
    }
}