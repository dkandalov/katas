// from http://bost.ocks.org/mike/path/

var n = 40;

function chart(domain, interpolation, myData, tick) {
    var margin = {top: 6, right: 0, bottom: 20, left: 40},
        width = 960 - margin.right,
        height = 320 - margin.top - margin.bottom;

    var x = d3.time.scale()
        .domain([myData[0].date, myData[myData.length - 1].date])
        .range([0, width]);

    var y = d3.scale.linear()
        .domain([0, d3.max(myData, function (d){ return d.open; })])
        .range([height, 0]);

    var line = d3.svg.line()
        .interpolate(interpolation)
        .x(function(d, i) { return x(d.date); })
        .y(function(d, i) { return y(d.open); });

    var svg = d3.select("body").append("p").append("svg")
        .attr("width", width + margin.left + margin.right)
        .attr("height", height + margin.top + margin.bottom)
        .style("margin-left", margin.left + "px")
        .append("g")
        .attr("transform", "translate(" + margin.left + "," + margin.top + ")");

    svg.append("defs").append("clipPath")
        .attr("id", "clip")
        .append("rect")
        .attr("width", width)
        .attr("height", height);

    var path = svg.append("g")
        .attr("clip-path", "url(#clip)")
        .append("path")
        .data([myData])
        .attr("class", "line")
        .attr("d", line);

    svg.append("g")
        .attr("class", "y axis")
        .call(d3.svg.axis().scale(y).ticks(10).orient("left"));

    svg.append("g")
        .attr("class", "x axis")
        .attr("transform", "translate(0," + (height) + ")")
        .call(d3.svg.axis().scale(x).tickSize(-height).tickSubdivide(false).tickFormat(d3.time.format("%d/%m/%y")).ticks(10).orient("bottom"));

//    tick(path, line, data, x);
}

//$.getJSON("/random", function (myData) {
$.getJSON("/quote/YHOO", function (myData) {
    var parse = d3.time.format("%d/%m/%Y").parse;
    var adaptedData = myData.v;
    adaptedData.forEach(function (d) {
        d.date = parse(d.date);
    });

    chart([0, n - 1], "linear", adaptedData, function tick(path, line, data, x) {
    //chart([1, n - 1], "basis", function tick(path, line, data, x) {
         // push a new data point onto the back
         data.push(myData);

         // redraw the line, and then slide it to the left
         path
         .attr("d", line)
         .attr("transform", null)
         .transition()
         .duration(750)
         .ease("linear")
         .attr("transform", "translate(" + x(-1) + ")")
         .each("end", function() { tick(path, line, data, x); });

         // pop the old data point off the front
         data.shift();
    });
});
