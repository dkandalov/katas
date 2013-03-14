describe("quick find", function () {
    var connections;

    beforeEach(function () {
        connections = new Array(10);
        for (var i = 0; i < connections.length; i++) {
            connections[i] = i;
        }
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
        var value = connections[p1];
        for (var i = 0; i < connections.length; i++) {
            if (connections[i] == value) {
                connections[i] = connections[p2];
            }
        }
    }

    function areConnected(p1, p2) {
        return connections[p1] == connections[p2];
    }
});