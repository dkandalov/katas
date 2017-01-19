describe("Binary Search Tree", function () {
    describe("traversal", function () {
        it("pre-order", function () {
            expect(traversePreOrder(null)).toEqual([]);
            expect(traversePreOrder({value: 123})).toEqual([123]);
            expect(traversePreOrder(bst())).toEqual([2, 1, 0.5, 1.5, 3, 2.5, 3.5]);
        });

        it("in-order", function () {
            expect(traverseInOrder(null)).toEqual([]);
            expect(traverseInOrder({value: 123})).toEqual([123]);
            expect(traverseInOrder(bst())).toEqual([0.5, 1, 1.5, 2, 2.5, 3, 3.5]);
        });

        it("post-order", function () {
            expect(traversePostOrder(null)).toEqual([]);
            expect(traversePostOrder({value: 123})).toEqual([123]);
            expect(traversePostOrder(bst())).toEqual([0.5, 1.5, 1, 2.5, 3.5, 3, 2]);
        });
    });

    it("convert to string", function () {
        expect(toString(null)).toEqual("()");
        expect(toString({value: 123})).toEqual("(123)");
        expect(toString(bst())).toEqual("(2 (1 (0.5) (1.5)) (3 (2.5) (3.5)))");
    });

    it("convert from string", function () {
        expect(parseTree("")).toEqual("null");
        expect(parseTree("()")).toEqual("null");
        expect(parseTree("(123)")).toEqual({value: 123});
        expect(parseTree("(2 (1 (0.5) (1.5)) (3 (2.5) (3.5)))")).toEqual(bst());
    });

    it("check that tree is valid", function () {
        expect(isBST(null)).toEqual(false);
        expect(isBST({value: 123})).toEqual(true);

        expect(isBST({value: 0, left: {value: 1}})).toEqual(false);
        expect(isBST({value: 1, left: {value: 0}})).toEqual(true);

        expect(isBST({value: 0, right: {value: 1}})).toEqual(true);
        expect(isBST({value: 1, right: {value: 0}})).toEqual(false);

        expect(isBST(bst())).toEqual(true);
    });

    it("min value", function () {
        expect(min(null)).toEqual(null);
        expect(min({value: 123})).toEqual(123);
        expect(min(bst())).toEqual(0.5);
    });

    it("max value", function () {
        expect(max(null)).toEqual(null);
        expect(max({value: 123})).toEqual(123);
        expect(max(bst())).toEqual(3.5);
    });

    it("contains an element", function () {
        expect(contains(null, 123)).toEqual(false);

        expect(contains({value: 123}, 123)).toEqual(true);
        expect(contains({value: 123}, 234)).toEqual(false);

        expect(contains(bst(), 0.5)).toEqual(true);
        expect(contains(bst(), 0.4)).toEqual(false);
        expect(contains(bst(), 3.5)).toEqual(true);
        expect(contains(bst(), 3.6)).toEqual(false);
    });

    it("add element", function () {
        expect(add(null, 123)).toEqual({value: 123});

        expect(add({value: 123}, 123)).toEqual({value: 123, left: {value: 123}});
        expect(add({value: 123}, 234)).toEqual({value: 123, right: {value: 234}});
        expect(add({value: 123}, 12)).toEqual({value: 123, left: {value: 12}});

        expect(find(add(bst(), 4), 3.5)).toEqual({value: 3.5, right: {value: 4}});
        expect(find(add(bst(), 1.1), 1.5)).toEqual({value: 1.5, left: {value: 1.1}});
    });

    it("remove element", function () {
        expect(remove(null, 123)).toEqual(null);

        expect(remove({value: 123}, 123)).toEqual(null);
        expect(remove({value: 123}, 234)).toEqual({value: 123, right: null});
        expect(remove({value: 123}, 12)).toEqual({value: 123, left: null});

        expect(remove({value: 123, left: {value: 12}}, 12)).toEqual({value: 123, left: null});
        expect(remove({value: 123, left: {value: 12}}, 123)).toEqual({value: 12});
        expect(remove({value: 123, right: {value: 234}}, 234)).toEqual({value: 123, right: null});
        expect(remove({value: 123, right: {value: 234}}, 123)).toEqual({value: 234});

        expect(remove({value: 123, left: {value: 12}, right: {value: 234}}, 12)).toEqual({
            value: 123,
            left: null,
            right: {value: 234}
        });
        expect(remove({value: 123, left: {value: 12}, right: {value: 234}}, 234)).toEqual({
            value: 123,
            left: {value: 12},
            right: null
        });
        expect(remove({value: 123, left: {value: 12}, right: {value: 234}}, 123)).toEqual({
            value: 234,
            left: {value: 12},
            right: null
        });

        // expect(remove(bst(), 0.1)).toEqual(bst());
        // expect(remove(bst(), 0.6)).toEqual(bst());
        // expect(remove(bst(), 4)).toEqual(bst());

        expect(find(remove(bst(), 0.5), 1)).toEqual({value: 1, left: null, right: {value: 1.5}});
        expect(find(remove(bst(), 1.5), 1)).toEqual({value: 1, left: {value: 0.5}, right: null});

        expect(find(remove(bst(), 3.5), 3)).toEqual({value: 3, left: {value: 2.5}, right: null});
        expect(find(remove(bst(), 2.5), 3)).toEqual({value: 3, left: null, right: {value: 3.5}});

        expect(remove(bst(), 3)).toEqual({
            value: 2,
            left: {
                value: 1,
                left: {value: 0.5},
                right: {value: 1.5}
            },
            right: {
                value: 3.5,
                left: {value: 2.5},
                right: null
            }
        });
    });


    function bst() {
        return {
            value: 2,
            left: {
                value: 1,
                left: {value: 0.5},
                right: {value: 1.5}
            },
            right: {
                value: 3,
                left: {value: 2.5},
                right: {value: 3.5}
            }
        };
    }
});
