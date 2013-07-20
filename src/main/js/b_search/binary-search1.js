describe("binary search", function () {
    it("finds index of element in array", function () {
        expect(search(1, [])).toEqual(-1);

        expect(search(0, [1])).toEqual(-1);
        expect(search(1, [1])).toEqual(0);
        expect(search(2, [1])).toEqual(-1);

        expect(search(0, [1, 2])).toEqual(-1);
        expect(search(1, [1, 2])).toEqual(0);
        expect(search(2, [1, 2])).toEqual(1);
        expect(search(3, [1, 2])).toEqual(-1);

        expect(search(0, [1, 2, 3])).toEqual(-1);
        expect(search(1, [1, 2, 3])).toEqual(0);
        expect(search(2, [1, 2, 3])).toEqual(1);
        expect(search(3, [1, 2, 3])).toEqual(2);
        expect(search(4, [1, 2, 3])).toEqual(-1);
    });
})

function search(value, array, shift) {
    if (array.length == 0) return -1;
    if (shift == null) shift = 0;

    const midIndex = Math.floor(array.length / 2);
    const midValue = array[midIndex];

    if (value == midValue) return midIndex + shift;
    else if (value < midValue) return search(value, array.slice(0, midIndex), shift);
    else return search(value, array.slice(midIndex + 1), shift + midIndex + 1);
}