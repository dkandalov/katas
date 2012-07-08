var t = 1297110663, // start time (seconds since epoch)
    v = 70,
    data = d3.range(33).map(next);

function next() {
    return {
        time: ++t,
        value: v = ~~Math.max(10, Math.min(90, v + 20 * (Math.random() - 0.5)))
    };
}
function redraw() {
    var rect = chart.selectAll("rect")
        .data(data, function(d) { return d.time; });

    rect.enter().insert("rect", "line")
        .attr("x", function(d, i) { return x(i) - 0.5; })
        .attr("y", function(d) { return h - y(d.value) - 0.5; })
        .attr("width", w)
        .attr("height", function(d) { return y(d.value); });

    rect.transition()
        .duration(1000)
        .attr("x", function(d, i) { return x(i) - 0.5; });

    rect.exit().remove();
}
setInterval(function() {
    data.shift();
    data.push(next());
    redraw();
}, 1000);

var w = 20, h = 80;
var x = d3.scale.linear()
    .domain([0, 1])
    .range([0, w]);
var y = d3.scale.linear()
    .domain([0, 100])
    .rangeRound([0, h]);

var chart = d3.select("body").append("svg")
    .attr("class", "chart")
    .attr("width", w * data.length - 1)
    .attr("height", h);

// bars
chart.selectAll("rect")
        .data(data)
    .enter().append("rect")
        .attr("x", function(d, i) { return x(i) - 0.5;} )
        .attr("y", function(d) { return h - y(d.value);} )
        .attr("width", w)
        .attr("height", function(d) { return y(d.value); });

// line at the bottom
chart.append("line")
    .attr("x1", 0)
    .attr("x2", w * data.length)
    .attr("y1", h - 0.5)
    .attr("y2", h - 0.5)
    .style("stroke", "#000");
