describe("quick find", function () {
    function Connections(size) {
        this.connections = new Array(size);
        for (var i = 0; i < this.connections.length; i++) {
            this.connections[i] = i;
        }
    }
    Connections.prototype.connect =  function (p1, p2) {
        var value = this.connections[p1];
        for (var i = 0; i < this.connections.length; i++) {
            if (this.connections[i] == value) {
                this.connections[i] = this.connections[p2];
            }
        }
    };
    Connections.prototype.areConnected = function(p1, p2) {
        return this.connections[p1] == this.connections[p2];
    };

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

    it("should know that connections are transitive", function () {
        connect(0, 1);
        connect(1, 2);
        expect(areConnected(0, 2)).toEqual(true);
    });

    it("should on complex example", function () {
        connect(3, 4);
        connect(4, 9);
        connect(8, 0);
        connect(2, 3);
        connect(5, 6);

        expect(areConnected(2, 9)).toEqual(true);
        connect(2, 9);
        connect(5, 9);
        connect(7, 3);
        connect(4, 8);

        expect(areConnected(5, 6)).toEqual(true);
        connect(5, 6);
        connect(0, 2);
        connect(6, 1);

        for (var i = 0; i < connections.length; i++) {
            for (var j = 0; j < connections.length; j++) {
                expect(areConnected(i, j)).toEqual(true);
            }
        }
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