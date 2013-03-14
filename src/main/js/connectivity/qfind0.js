describe("quick find", function () {
    function QuickFind(size) {
        this.size = size;
        this.connections = new Array(size);
        for (var i = 0; i < this.connections.length; i++) {
            this.connections[i] = i;
        }
    }
    QuickFind.prototype.connect =  function (p1, p2) {
        var value = this.connections[p1];
        for (var i = 0; i < this.connections.length; i++) {
            if (this.connections[i] == value) {
                this.connections[i] = this.connections[p2];
            }
        }
    };
    QuickFind.prototype.areConnected = function(p1, p2) {
        return this.connections[p1] == this.connections[p2];
    };

    var quickFind;

    beforeEach(function () {
        quickFind = new QuickFind(10);
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
        function assertAllPointsAreConnected(value) {
            for (var i = 0; i < quickFind.size; i++) {
                for (var j = 0; j < quickFind.size; j++) {
                    if (i == j) continue;
                    expect(areConnected(i, j)).toEqual(value);
                }
            }
        }
        assertAllPointsAreConnected(false);

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

        assertAllPointsAreConnected(true);
    });

    function connect(p1, p2) {
        quickFind.connect(p1, p2);
    }

    function areConnected(p1, p2) {
        return quickFind.areConnected(p1, p2);
    }
});