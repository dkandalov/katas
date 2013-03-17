describe("merge sort(v1)", function() {
    it("should sort array", function() {
        expect(mergeSort([])).toEqual([]);
        expect(mergeSort([1])).toEqual([1]);
        expect(mergeSort([1, 2])).toEqual([1, 2]);
        expect(mergeSort([2, 1])).toEqual([1, 2]);

        expect(mergeSort([1, 2, 3])).toEqual([1, 2, 3]);
        expect(mergeSort([1, 3, 2])).toEqual([1, 2, 3]);
        expect(mergeSort([2, 1, 3])).toEqual([1, 2, 3]);
        expect(mergeSort([2, 3, 1])).toEqual([1, 2, 3]);
        expect(mergeSort([3, 1, 2])).toEqual([1, 2, 3]);
        expect(mergeSort([3, 2, 1])).toEqual([1, 2, 3]);
    });

    function mergeSort(values) {
        if (values.length < 2) return values;
        var parts = split(values);
        return merge(mergeSort(parts[0]), mergeSort(parts[1]));
    }

    function split(values) {
        const middle = Math.floor(values.length / 2);
        return [values.slice(0, middle), values.slice(middle, values.length)];
    }

    function merge(part1, part2) {
        var result = [];
        var i1 = 0, i2 = 0;
        while (i1 < part1.length && i2 < part2.length) {
            if (part1[i1] <= part2[i2]) {
                result = result.concat(part1[i1++]);
            } else {
                result = result.concat(part2[i2++]);
            }
        }
        while (i1 < part1.length) result = result.concat(part1[i1++]);
        while (i2 < part2.length) result = result.concat(part2[i2++]);
        return result;
    }
});