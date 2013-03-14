describe("quick find", function () {
    var connections = new Array(10);

    it("should determine if points are connected", function () {
        expect(areConnected(0, 0)).toEqual(true);
        expect(areConnected(0, 1)).toEqual(false);
    });

    function areConnected(p1, p2) {
        return true;
    }
});