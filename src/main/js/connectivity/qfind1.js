describe("quick find: ", function () {
    it("connections are reflexive", function () {
        expect(areConnected(0, 0)).toEqual(true);
    });

    it("connections are symmetric", function () {
        expect(areConnected(0, 1)).toEqual(false);
        expect(areConnected(1, 0)).toEqual(false);
        connect(0, 1);
        expect(areConnected(0, 1)).toEqual(true);
        expect(areConnected(1, 0)).toEqual(true);
    });

    it("connections are transitive", function () {
        expect(areConnected(0, 1)).toEqual(false);
        expect(areConnected(1, 2)).toEqual(false);
        connect(0, 1);
        connect(1, 2);
        expect(areConnected(0, 1)).toEqual(true);
        expect(areConnected(1, 2)).toEqual(true);
        expect(areConnected(0, 2)).toEqual(true);
    });

    it("should support reconnection", function () {
        connect(0, 1);
        connect(1, 2);
        expect(areConnected(0, 2)).toEqual(true);
        connect(1, 3);
        expect(areConnected(0, 2)).toEqual(false);
        expect(areConnected(0, 3)).toEqual(true);
    });

    it("TODO", function () {

    });



    var data = [];

    beforeEach(function () {
        for (var i = 0; i < 10; i++) {
            data[i] = i;
        }
    });

    function areConnected(p1, p2) {
        return rootOf(p1) == rootOf(p2);
    }

    function rootOf(p) {
        if (data[p] == p) return p;
        else return rootOf(data[p]);
    }

    function connect(p1, p2) {
        data[p1] = data[p2];
    }
});