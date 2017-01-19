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

    it("should work with circular connections", function () {
        connect(0, 1);
        connect(1, 2);
        connect(2, 0);
        expect(areConnected(0, 1)).toEqual(true);
        expect(areConnected(1, 2)).toEqual(true);
        expect(areConnected(2, 0)).toEqual(true);
    });


    var data = [];

    beforeEach(function () {
        for (var i = 0; i < 10; i++) {
            data[i] = i;
        }
    });

    function areConnected(p1, p2) {
        return data[p1] == data[p2];
    }

    function connect(p1, p2) {
        const p1Root = rootOf(p1);
        const p2Root = rootOf(p2);
        for (var i = 0; i < 10; i++) {
            if (data[i] == p1Root) {
                data[i] = p2Root;
            }
        }
    }

    function rootOf(p) {
        if (data[p] == p) return p;
        else return rootOf(data[p]);
    }
});