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
        if (part1.length == 0) return part2;
        if (part2.length == 0) return part1;

        if (part1[0] < part2[0]) {
            return [part1[0]].concat(merge(part1.slice(1, part1.length), part2));
        } else {
            return [part2[0]].concat(merge(part1, part2.slice(1, part2.length)));
        }
    }
});