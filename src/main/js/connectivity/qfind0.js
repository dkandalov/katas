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

    it("should know if two different points are connected", function () {
        expect(areConnected(0, 1)).toEqual(false);
        connect(0, 1);
        expect(areConnected(0, 1)).toEqual(true);
    });

    function connect(p1, p2) {

    }

    function areConnected(p1, p2) {
        if (p1 == p2) return true;
        return false;
    }
});