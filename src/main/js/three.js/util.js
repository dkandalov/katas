function xyzKeyController(target, stepSize, onUpdate) {
    if (stepSize === undefined) stepSize = 100;
    var dx, dy, dz;
    if (_.isArray(stepSize)) {
        dx = stepSize[0];
        dy = stepSize[1];
        dz = stepSize[2];
    } else {
        dx = stepSize;
        dy = stepSize;
        dz = stepSize;
    }
    if (onUpdate !== undefined) onUpdate();

    var j = 106, l = 108, k = 107, i = 105, u = 117, o = 111;
    return function (event) {
        var key = (event.key !== undefined ? event.key : event.keyCode);
        if (key === j) target.setX(target.getX() - dx);
        if (key === l) target.setX(target.getX() + dx);
        if (key === k) target.setY(target.getY() - dy);
        if (key === i) target.setY(target.getY() + dy);
        if (key === u) target.setZ(target.getZ() - dz);
        if (key === o) target.setZ(target.getZ() + dz);
        if (onUpdate !== undefined && _.contains([j, l, k, i, u, o], key)) {
            onUpdate();
        }
    }
}

function newSpherePosition(delegate, radius, x, y, z) {
    var r = radius === undefined ? 100 : radius;
    var theta = 0;
    var phi = 0;
    function update() {
        delegate.x = x + r * Math.cos(theta) * Math.sin(phi);
        delegate.y = y + r * Math.sin(theta) * Math.sin(phi);
        delegate.z = z + r * Math.cos(phi);
    }
    update();
    return {
        getX: function () { return theta; },
        getY: function () { return phi; },
        getZ: function () { return r; },
        setX: function (value) {
            theta = value;
            update();
        },
        setY: function (value) {
            phi = value;
            update();
        },
        setZ: function (value) {
            r = value;
            update();
        }
    };
}