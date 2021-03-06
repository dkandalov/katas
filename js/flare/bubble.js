var r = 960,
    format = d3.format(",d"),
    fill = d3.scale.category20c();

var bubble = d3.layout.pack()
    .sort(null)
    .size([r, r]);

var vis = d3.select("#chart").append("svg")
    .attr("width", r)
    .attr("height", r)
    .attr("class", "bubble");

d3.json("flare.json", function(json) {
    var nodes = vis.selectAll("g.node")
        .data(bubble.nodes(classes(json))
        .filter(function(d) { return !d.children; }))
        .enter().append("g")
        .attr("class", "node")
        .attr("transform", function(d) { return "translate(" + d.x + "," + d.y + ")"; });

    nodes.append("title")
         .text(function(d) { return d.className + ": " + format(d.value); });

    nodes.append("circle")
         .attr("r", function(d) { return d.r; })
         .style("fill", function(d) { return fill(d.packageName); });

    nodes.append("text")
         .attr("text-anchor", "middle")
         .attr("dy", ".3em")
         .text(function(d) { return d.className.substring(0, d.r / 3); });
});

// Returns a flattened hierarchy containing all leaf nodes under the root.
function classes(root) {
    var classes = [];

    function recurse(name, node) {
        if (node.children) node.children.forEach(function(child) { recurse(node.name, child); });
        else classes.push({packageName: name, className: node.name, value: node.size});
    }

    recurse(null, root);
    return {children: classes};
}