describe("binary search", function() {
    it("should just work...", function() {
        expect(bsearch(1, [])).toEqual(-1);

        expect(bsearch(0, [1])).toEqual(-1);
        expect(bsearch(1, [1])).toEqual(0);
        expect(bsearch(2, [1])).toEqual(-1);

        expect(bsearch(0, [1, 2])).toEqual(-1);
        expect(bsearch(1, [1, 2])).toEqual(0);
        expect(bsearch(2, [1, 2])).toEqual(1);
        expect(bsearch(3, [1, 2])).toEqual(-1);

        expect(bsearch(0, [1, 2, 3])).toEqual(-1);
        expect(bsearch(1, [1, 2, 3])).toEqual(0);
        expect(bsearch(2, [1, 2, 3])).toEqual(1);
        expect(bsearch(3, [1, 2, 3])).toEqual(2);
        expect(bsearch(4, [1, 2, 3])).toEqual(-1);

        expect(bsearch(0, [1, 2, 3, 4])).toEqual(-1);
        expect(bsearch(1, [1, 2, 3, 4])).toEqual(0);
        expect(bsearch(2, [1, 2, 3, 4])).toEqual(1);
        expect(bsearch(3, [1, 2, 3, 4])).toEqual(2);
        expect(bsearch(4, [1, 2, 3, 4])).toEqual(3);
        expect(bsearch(5, [1, 2, 3, 4])).toEqual(-1);
    });

    function bsearch(value, array) {
        var from = 0;
        var to = array.length;
        while (from < to) {
            var midPos = Math.floor((from + to) / 2);
            var midValue = array[midPos];
            if (value < midValue) {
                to = midPos;
            } else if (value > midValue) {
                from = midPos + 1;
            } else {
                return midPos;
            }
        }
        return -1;
    }
});
