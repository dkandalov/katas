describe("quick find", function () {
    var connections;

    beforeEach(function () {
        connections = new Array(10);
    });

    it("should know that points are connected to themselves", function () {
        expect(areConnected(0, 0)).toEqual(true);
        expect(areConnected(1, 1)).toEqual(true);

        expect(areConnected(0, 1)).toEqual(false);
        expect(areConnected(1, 0)).toEqual(false);
    });

    function areConnected(p1, p2) {
        if (p1 == p2) return true;
        return false;
    }
});