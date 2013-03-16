describe("merge sort", function () {
    it("should sort arrays", function () {
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
    })

    it("should split array into two parts", function () {
        expect(split([])).toEqual([[], []]);
        expect(split([1])).toEqual([[], [1]]);
        expect(split([1, 2])).toEqual([[1], [2]]);
        expect(split([1, 2, 3])).toEqual([[1], [2, 3]]);
        expect(split([1, 2, 3, 4])).toEqual([[1, 2], [3, 4]]);
    });

    function mergeSort(values) {
        if (values.length < 2) {
            return values;
        } else {
            var parts = split(values);
            return merge(mergeSort(parts[0]), mergeSort(parts[1]));
        }
    }

    function merge(part1, part2) {
        if (part1.length == 0) return part2;
        if (part2.length == 0) return part1;

        if (part1[0] < part2[0])
            return [part1[0]].concat(merge(part1.slice(1, part1.length), part2));
        else
            return [part2[0]].concat(merge(part1, part2.slice(1, part2.length)));
    }

    function split(values) {
        var middle = Math.floor(values.length / 2)
        return [values.slice(0, middle), values.slice(middle, values.length)];
    }
});