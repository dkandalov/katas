describe("Quick find: ", function () {
    it("connections should be reflexive", function () {
        expect(quickFind.areConnected(0, 0)).toEqual(true);
        expect(quickFind.areConnected(1, 1)).toEqual(true);
    });

    it("connections should symmetric", function () {
        expect(quickFind.areConnected(0, 1)).toEqual(false);
        expect(quickFind.areConnected(1, 0)).toEqual(false);
        quickFind.connect(0, 1);
        expect(quickFind.areConnected(0, 1)).toEqual(true);
        expect(quickFind.areConnected(1, 0)).toEqual(true);
    });

    it("connections should be transitive", function () {
        expect(quickFind.areConnected(0, 2)).toEqual(false);
        quickFind.connect(0, 1);
        quickFind.connect(1, 2);
        expect(quickFind.areConnected(0, 2)).toEqual(true);
    });

    it("connections don't have loops", function () {
        quickFind.connect(0, 1);
        quickFind.connect(1, 2);
        quickFind.connect(2, 0);
        expect(quickFind.areConnected(0, 2)).toEqual(true);
    });


    function QuickFind(size) {
        this.size = size;
        this.data = [];
        for (var i = 0; i < size; i++) {
            this.data[i] = i;
        }
    }
    QuickFind.prototype.connect = function (p1, p2) {
        var data = this.data;
        function rootOf(p) { return data[p] == p ? p : rootOf(data[p]); }

        var p1Root = rootOf(p1);
        var p2Root = rootOf(p2);
        for (var i = 0; i < this.size; i++) {
            if (this.data[i] == p1Root) this.data[i] = p2Root;
        }
    };

    QuickFind.prototype.areConnected = function (p1, p2) {
        return this.data[p1] == this.data[p2];
    };


    var quickFind;

    beforeEach(function () {
        quickFind = new QuickFind(10);
    });
});