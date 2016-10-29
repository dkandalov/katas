var tree = {
    value: 0,
    left: {
        value: 1,
        left: { value: 3 },
        right: { value: 4 }
    },
    right: {
        value: 2,
        left: { value: 5 },
        right: { value: 6 }
    }
};

function traversePreOrder(tree) {
    if (!tree) return [];
    return [].concat([tree.value])
             .concat(traversePreOrder(tree.left))
             .concat(traversePreOrder(tree.right));
}
function traverseInOrder(tree) {
    if (!tree) return [];
    return [].concat(traverseInOrder(tree.left))
             .concat([tree.value])
             .concat(traverseInOrder(tree.right));
}
function traversePostOrder(tree) {
    if (!tree) return [];
    return [].concat(traversePostOrder(tree.left))
             .concat(traversePostOrder(tree.right))
             .concat([tree.value]);
}

function toString(tree) {
    if (!tree) return "()";
    var childrenAsString = "";
    if (tree.left || tree.right) {
        childrenAsString = " " + toString(tree.left) + " " + toString(tree.right)
    }
    return "(" + tree.value + childrenAsString + ")";
}

var bst = {
    value: 2,
    left: {
        value: 1,
        left: { value: 0.5 },
        right: { value: 1.5 }
    },
    right: {
        value: 3,
        left: { value: 2.5 },
        right: { value: 3.5 }
    }
};

function isBST(tree) {
    if (!tree) return false;
    var leftIsOk = !tree.left || (tree.left.value <= tree.value && isBST(tree.left));
    var rightIsOk = !tree.right || (tree.right.value > tree.value && isBST(tree.right));
    return leftIsOk && rightIsOk
}

// traversePreOrder(bst)
// [2, 1, 0.5, 1.5, 3, 2.5, 3.5]
// traversePostOrder(bst)
// [0.5, 1.5, 1, 2.5, 3.5, 3, 2]
// traverseInOrder(bst)
// [0.5, 1, 1.5, 2, 2.5, 3, 3.5]

function min(tree) {
    if (!tree) return null;
    return tree.left ? min(tree.left) : tree.value;
}

function max(tree) {
    if (!tree) return null;
    return tree.right ? max(tree.right) : tree.value;
}

function find(tree, value) {
    if (!tree) return null;
    if (value == tree.value) return tree;
    else if (value < tree.value) return find(tree.left, value);
    else if (value > tree.value) return find(tree.right, value);
}

function contains(tree, value) {
    return find(tree, value) != null;
}

function add(tree, value) {
    if (!tree) return { value: value };

    if (value <= tree.value) {
        tree.left = add(tree.left, value);
    } else if (value > tree.value) {
        tree.right = add(tree.right, value);
    }
    return tree;
}

function remove(tree, value) {
    if (!tree) return null;
    if (value < tree.value) {
        tree.left = remove(tree.left, value);
        return tree;
    } else if (value > tree.value) {
        tree.right = remove(tree.right, value);
        return tree;
    } else {
        if (!tree.left && !tree.right) return null;
        else if (tree.left && !tree.right) return tree.left;
        else if (!tree.left && tree.right) return tree.right;
        else {
            tree.value = min(tree.right);
            tree.right = remove(tree.right, tree.value);
            return tree;
        }
    }
}